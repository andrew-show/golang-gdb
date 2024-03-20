# Copyright 2010 The Go Authors.
# Copyright 2024 Andrew Chow <andrew_show@hotmail.com>
# All rights reserved.
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#   * Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above
# copyright notice, this list of conditions and the following disclaimer
# in the documentation and/or other materials provided with the
# distribution.
#    * Neither the name of Google Inc. nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""GDB Pretty printers and convenience functions for Go's runtime structures.

This script is loaded by GDB when it finds a .debug_gdb_scripts
section in the compiled binary. The [68]l linkers emit this with a
path to this file based on the path to the runtime package.
"""

# Known issues:
#    - pretty printing only works for the 'native' strings. E.g. 'type
#      foo string' will make foo a plain struct in the eyes of gdb,
#      circumventing the pretty print triggering.


from __future__ import print_function
import re
import sys
import gdb
import gdb.unwinder


print("Loading Go Runtime support.", file=sys.stderr)
#http://python3porting.com/differences.html
if sys.version > '3':
        xrange = range
# allow to manually reload while developing
goobjfile = gdb.current_objfile() or gdb.objfiles()[0]
goobjfile.pretty_printers = []

# G state (runtime2.go)

def read_runtime_const(varname, default):
        try:
                return int(gdb.parse_and_eval(varname))
        except Exception:
                return int(default)

#
#  Value wrappers
#

class SliceValue:
        "Wrapper for slice values."

        def __init__(self, val):
                self.val = val

        @property
        def len(self):
                return int(self.val['len'])

        @property
        def cap(self):
                return int(self.val['cap'])

        def __getitem__(self, i):
                if i < 0 or i >= self.len:
                        raise IndexError(i)
                ptr = self.val["array"]
                return (ptr + i).dereference()


#
#  Pretty Printers
#

# The patterns for matching types are permissive because gdb 8.2 switched to matching on (we think) typedef names instead of C syntax names.
class StringTypePrinter:
        "Pretty print Go strings."

        pattern = re.compile(r'^(struct string( \*)?|string)$')

        def __init__(self, val):
                self.val = val

        def display_hint(self):
                return 'string'

        def to_string(self):
                l = int(self.val['len'])
                return self.val['str'].string("utf-8", "ignore", l)


class SliceTypePrinter:
        "Pretty print slices."

        pattern = re.compile(r'^(struct \[\]|\[\])')

        def __init__(self, val):
                self.val = val

        def display_hint(self):
                return 'array'

        def to_string(self):
                t = str(self.val.type)
                if (t.startswith("struct ")):
                        return t[len("struct "):]
                return t

        def children(self):
                sval = SliceValue(self.val)
                if sval.len > sval.cap:
                        return
                for idx, item in enumerate(sval):
                        yield ('[{0}]'.format(idx), item)


class MapTypePrinter:
        """Pretty print map[K]V types.

        Map-typed go variables are really pointers. dereference them in gdb
        to inspect their contents with this pretty printer.
        """

        pattern = re.compile(r'^map\[.*\].*$')

        def __init__(self, val):
                self.val = val

        def display_hint(self):
                return 'map'

        def to_string(self):
                return str(self.val.type)

        def children(self):
                MapBucketCount = 8 # see internal/abi.go:MapBucketCount
                B = self.val['B']
                buckets = self.val['buckets']
                oldbuckets = self.val['oldbuckets']
                flags = self.val['flags']
                inttype = self.val['hash0'].type
                cnt = 0
                for bucket in xrange(2 ** int(B)):
                        bp = buckets + bucket
                        if oldbuckets:
                                oldbucket = bucket & (2 ** (B - 1) - 1)
                                oldbp = oldbuckets + oldbucket
                                oldb = oldbp.dereference()
                                if (oldb['overflow'].cast(inttype) & 1) == 0:  # old bucket not evacuated yet
                                        if bucket >= 2 ** (B - 1):
                                                continue    # already did old bucket
                                        bp = oldbp
                        while bp:
                                b = bp.dereference()
                                for i in xrange(MapBucketCount):
                                        if b['tophash'][i] != 0:
                                                k = b['keys'][i]
                                                v = b['values'][i]
                                                if flags & 1:
                                                        k = k.dereference()
                                                if flags & 2:
                                                        v = v.dereference()
                                                yield str(cnt), k
                                                yield str(cnt + 1), v
                                                cnt += 2
                                bp = b['overflow']


class ChanTypePrinter:
        """Pretty print chan[T] types.

        Chan-typed go variables are really pointers. dereference them in gdb
        to inspect their contents with this pretty printer.
        """

        pattern = re.compile(r'^chan ')

        def __init__(self, val):
                self.val = val

        def display_hint(self):
                return 'array'

        def to_string(self):
                return str(self.val.type)

        def children(self):
                # see chan.c chanbuf(). et is the type stolen from hchan<T>::recvq->first->elem
                et = [x.type for x in self.val['recvq']['first'].type.target().fields() if x.name == 'elem'][0]
                ptr = (self.val.address["buf"]).cast(et)
                for i in range(self.val["qcount"]):
                        j = (self.val["recvx"] + i) % self.val["dataqsiz"]
                        yield ('[{0}]'.format(i), (ptr + j).dereference())


def paramtypematch(t, pattern):
        return t.code == gdb.TYPE_CODE_TYPEDEF and str(t).startswith(".param") and pattern.match(str(t.target()))

#
#  Register all the *Printer classes above.
#

def makematcher(klass):
        def matcher(val):
                try:
                        if klass.pattern.match(str(val.type)):
                                return klass(val)
                        elif paramtypematch(val.type, klass.pattern):
                                return klass(val.cast(val.type.target()))
                except Exception:
                        pass
        return matcher

goobjfile.pretty_printers.extend([makematcher(var) for var in vars().values() if hasattr(var, 'pattern')])
#
#  Utilities
#


#
#  For reference, this is what we're trying to do:
#  eface: p *(*(struct 'runtime.rtype'*)'main.e'->type_->data)->string
#  iface: p *(*(struct 'runtime.rtype'*)'main.s'->tab->Type->data)->string
#
# interface types can't be recognized by their name, instead we check
# if they have the expected fields.  Unfortunately the mapping of
# fields to python attributes in gdb.py isn't complete: you can't test
# for presence other than by trapping.


def is_iface(val):
        try:
                return str(val['tab'].type) == "struct runtime.itab *" and str(val['data'].type) == "void *"
        except gdb.error:
                pass


def is_eface(val):
        try:
                return str(val['_type'].type) == "struct runtime._type *" and str(val['data'].type) == "void *"
        except gdb.error:
                pass


def lookup_type(name):
        try:
                return gdb.lookup_type(name)
        except gdb.error:
                pass
        try:
                return gdb.lookup_type('struct ' + name)
        except gdb.error:
                pass
        try:
                return gdb.lookup_type('struct ' + name[1:]).pointer()
        except gdb.error:
                pass


def iface_commontype(obj):
        if is_iface(obj):
                go_type_ptr = obj['tab']['_type']
        elif is_eface(obj):
                go_type_ptr = obj['_type']
        else:
                return

        return go_type_ptr.cast(gdb.lookup_type("struct reflect.rtype").pointer()).dereference()


def iface_dtype(obj):
        "Decode type of the data field of an eface or iface struct."
        # known issue: dtype_name decoded from runtime.rtype is "nested.Foo"
        # but the dwarf table lists it as "full/path/to/nested.Foo"

        dynamic_go_type = iface_commontype(obj)
        if dynamic_go_type is None:
                return
        dtype_name = dynamic_go_type['string'].dereference()['str'].string()

        dynamic_gdb_type = lookup_type(dtype_name)
        if dynamic_gdb_type is None:
                return

        type_size = int(dynamic_go_type['size'])
        uintptr_size = int(dynamic_go_type['size'].type.sizeof)  # size is itself a uintptr
        if type_size > uintptr_size:
                        dynamic_gdb_type = dynamic_gdb_type.pointer()

        return dynamic_gdb_type


def iface_dtype_name(obj):
        "Decode type name of the data field of an eface or iface struct."

        dynamic_go_type = iface_commontype(obj)
        if dynamic_go_type is None:
                return
        return dynamic_go_type['string'].dereference()['str'].string()


class IfacePrinter:
        """Pretty print interface values

        Casts the data field to the appropriate dynamic type."""

        def __init__(self, val):
                self.val = val

        def display_hint(self):
                return 'string'

        def to_string(self):
                if self.val['data'] == 0:
                        return 0x0
                try:
                        dtype = iface_dtype(self.val)
                except Exception:
                        return "<bad dynamic type>"

                if dtype is None:  # trouble looking up, print something reasonable
                        return "({typename}){data}".format(
                                typename=iface_dtype_name(self.val), data=self.val['data'])

                try:
                        return self.val['data'].cast(dtype).dereference()
                except Exception:
                        pass
                return self.val['data'].cast(dtype)


def ifacematcher(val):
        if is_iface(val) or is_eface(val):
                return IfacePrinter(val)

goobjfile.pretty_printers.append(ifacematcher)

#
#  Convenience Functions
#


class GoLenFunc(gdb.Function):
        "Length of strings, slices, maps or channels"

        how = ((StringTypePrinter, 'len'), (SliceTypePrinter, 'len'), (MapTypePrinter, 'count'), (ChanTypePrinter, 'qcount'))

        def __init__(self):
                gdb.Function.__init__(self, "len")

        def invoke(self, obj):
                typename = str(obj.type)
                for klass, fld in self.how:
                        if klass.pattern.match(typename) or paramtypematch(obj.type, klass.pattern):
                                return obj[fld]


class GoCapFunc(gdb.Function):
        "Capacity of slices or channels"

        how = ((SliceTypePrinter, 'cap'), (ChanTypePrinter, 'dataqsiz'))

        def __init__(self):
                gdb.Function.__init__(self, "cap")

        def invoke(self, obj):
                typename = str(obj.type)
                for klass, fld in self.how:
                        if klass.pattern.match(typename) or paramtypematch(obj.type, klass.pattern):
                                return obj[fld]


class DTypeFunc(gdb.Function):
        """Cast Interface values to their dynamic type.

        For non-interface types this behaves as the identity operation.
        """

        def __init__(self):
                gdb.Function.__init__(self, "dtype")

        def invoke(self, obj):
                try:
                        return obj['data'].cast(iface_dtype(obj))
                except gdb.error:
                        pass
                return obj

#
#  Commands
#

def linked_list(ptr, linkfield):
        while ptr:
                yield ptr
                ptr = ptr[linkfield]

class Goroutine:
        IDLE = read_runtime_const("'runtime._Gidle'", 0)
        RUNNABLE = read_runtime_const("'runtime._Grunnable'", 1)
        RUNNING = read_runtime_const("'runtime._Grunning'", 2)
        SYSCALL = read_runtime_const("'runtime._Gsyscall'", 3)
        WAITING = read_runtime_const("'runtime._Gwaiting'", 4)
        MORIBUND_UNUSED = read_runtime_const("'runtime._Gmoribund_unused'", 5)
        DEAD = read_runtime_const("'runtime._Gdead'", 6)
        ENQUEUE_UNUSED = read_runtime_const("'runtime._Genqueue_unused'", 7)
        COPYSTACK = read_runtime_const("'runtime._Gcopystack'", 8)
        SCAN = read_runtime_const("'runtime._Gscan'", 0x1000)
        SCANRUNNABLE = SCAN+RUNNABLE
        SCANRUNNING = SCAN+RUNNING
        SCANSYSCALL = SCAN+SYSCALL
        SCANWAITING = SCAN+WAITING

        STATUS_TEXT = {
                IDLE: 'idle',
                RUNNABLE: 'runnable',
                RUNNING: 'running',
                SYSCALL: 'syscall',
                WAITING: 'waiting',
                MORIBUND_UNUSED: 'moribund',
                DEAD: 'dead',
                ENQUEUE_UNUSED: 'enqueue',
                COPYSTACK: 'copystack',
                SCAN: 'scan',
                SCANRUNNABLE: 'runnable+s',
                SCANRUNNING: 'running+s',
                SCANSYSCALL: 'syscall+s',
                SCANWAITING: 'waiting+s',
        }

        SIZEOF_ADDRESS = gdb.lookup_type("void").pointer().sizeof

        def __init__(self, base):
                self.__g = base

        @property
        def id(self):
                return int(self.__g['goid'])

        @property
        def status(self):
                try:
                        return int(self.__g['atomicstatus']['value']) & ~Goroutine.SCAN
                except gdb.error as e:
                        return int(self.__g['atomicstatus']) & ~Goroutine.SCAN

        @property
        def thread(self):
                status = self.status
                if status == Goroutine.RUNNING or status == Goroutine.SYSCALL:
                        pid = self.__g['m']['procid']
                        for thread in gdb.selected_inferior().threads():
                                if thread.ptid[1] == pid:
                                        return thread
                return None

        @property
        def pc(self):
                return self.__g['sched']['pc']

        @property
        def sp(self):
                return self.__g['sched']['sp']

        @property
        def bp(self):
                return self.__g['sched']['bp']

        def print(self, title="Goroutine"):
                status = self.status
                if status == Goroutine.DEAD:
                        print("{} {:>4} ({})".format(
                                title,
                                self.id,
                                Goroutine.format_status(status)))
                elif status != Goroutine.RUNNING and status != Goroutine.SYSCALL:
                        # Goroutine is not running nor in syscall, so use the info in goroutine
                        pc, sp, bp = self.pc, self.sp, self.bp

                        print("{} {:>4} ({} PC={} SP={} BP={})".format(
                                title,
                                self.id,
                                Goroutine.format_status(status),
                                Goroutine.format_address(pc),
                                Goroutine.format_address(sp),
                                Goroutine.format_address(bp)))
                else:
                        thread = self.thread
                        print("{} {:>4} ({} thread {} (LWP {}))".format(
                                title,
                                self.id,
                                Goroutine.format_status(status),
                                thread.global_num,
                                thread.ptid[1]))

        @classmethod
        def format_status(cls, status):
                return cls.STATUS_TEXT.get(status, "unknown ({})".format(status))

        @classmethod
        def format_address(cls, address):
                return "0x{:x}".format(int(address)).rjust(cls.SIZEOF_ADDRESS << 1, '0')

        @classmethod
        def all(cls):
                class Goroutines:
                        def __init__(self):
                                self.__allgs = gdb.lookup_global_symbol('runtime.allgs').value()

                        def __len__(self):
                                return int(self.__allgs['len'])

                        def __getitem__(self, i):
                                if i < 0 or i >= self.__len__():
                                        raise IndexError(i)
                                return Goroutine((self.__allgs["array"] + i).dereference())

                return Goroutines()

        @classmethod
        def get(cls, goid):
                allgs = gdb.lookup_global_symbol('runtime.allgs').value()
                for i in range(int(allgs['len'])):
                        g = (allgs['array'] + i).dereference()
                        if g['goid'] == goid:
                               return Goroutine(g)
                return None


class GoroutinesCmd(gdb.Command):
        "List all goroutines."

        def __init__(self):
                gdb.Command.__init__(self, "info goroutines", gdb.COMMAND_STACK, gdb.COMPLETE_NONE)

        def invoke(self, _arg, _from_tty):
                # args = gdb.string_to_argv(arg)
                # args = gdb.string_to_argv(arg)
                all = Goroutine.all()
                print("Total {} goroutines".format(len(all)))
                print()

                for g in all:
                      g.print()


class GoroutineContext:
        def __init__(self, g):
                self.__g = g
                self.__current = None

        def __enter__(self):
                class Unwinder(gdb.unwinder.Unwinder):
                        def __init__(self, g):
                                super().__init__("Goroutine")
                                self.__g = g

                        def __call__(self, pending_frame):
                                if pending_frame.level() != 0:
                                        return None

                                class FrameID:
                                        def __init__(self, sp, pc):
                                                self.sp = sp
                                                self.pc = pc

                                sp, pc = self.__g.sp, self.__g.pc

                                # create unwind_info with different sp
                                frame_id = FrameID(sp - 32, pc)
                                unwind_info = pending_frame.create_unwind_info(frame_id)

                                # And store the previous values into our cache.
                                unwind_info.add_saved_register("sp", sp)
                                unwind_info.add_saved_register("pc", pc)

                                # Return the cache for GDB to use.
                                return unwind_info

                self.__current = gdb.selected_thread()
                status = self.__g.status
                if status == Goroutine.DEAD:
                        return None
                elif status == Goroutine.RUNNING or status == Goroutine.SYSCALL:
                        thread = self.__g.thread
                        if thread is None:
                                return None

                        thread.switch()
                else:
                        for thread in gdb.selected_inferior().threads():
                                thread.switch()
                                frame = gdb.newest_frame()
                                name = frame.name()
                                if name in ("runtime.futex", "runtime.mcall", "runtime.schedule", "runtime.findRunnable", "runtime.futexsleep"):
                                        break
                        else:
                                thread = gdb.selected_thread()

                        gdb.unwinder.register_unwinder(None, Unwinder(self.__g), replace=True)

                return self

        def __exit__(self, type, value, trace):
                class Unwinder(gdb.unwinder.Unwinder):
                        def __init__(self):
                                super().__init__("Goroutine")

                        def __call__(self, pending_frame):
                                return None

                gdb.unwinder.register_unwinder(None, Unwinder(), replace=True)
                self.__current.switch()

        def select_frame(self, level):
                gdb.execute("select-frame {}".format(level), False, True)


class GoroutineCmd(gdb.Command):
        """Switch to the context of goroutine, and execute arbitrary gdb command.

Usage:
    goroutine -- Display current goroutine
    goroutine <goid> -- Select goroutine by goid
    goroutine <gdb command> -- Execute gdb command for selected goroutine
    goroutine all <gdb command> -- Apply gdb command to all goroutines

Example: Print backtrace of all goroutines.
    (gdb) goroutine all bt

Example: Select a goroutine and print backtrace.
    (gdb) goroutine 123
    (gdb) goroutine bt

Example: Select a goroutine and examine local variable of specific frame.
    (gdb) goroutine 123
    (gdb) goroutine frame 4
    (gdb) goroutine info local
        """

        def __init__(self):
                gdb.Command.__init__(self, "goroutine", gdb.COMMAND_STACK, gdb.COMPLETE_COMMAND)
                self.__g = None
                self.__frame = None

        def invoke(self, arg, _from_tty):
                args = gdb.string_to_argv(arg)
                if len(args) < 1:
                        if self.__g is None:
                                print("No Goroutine is selected")
                        else:
                                self.__g.print("Current Goroutine")
                elif args[0].isdigit():
                        goid = int(args[0])
                        g = Goroutine.get(goid)
                        if g is None:
                                print("Invalid Goroutine ID {}".format(goid))
                        elif g.status == Goroutine.DEAD:
                                print("Can not switch to the dead Goroutine {}".format(goid))
                        else:
                                self.__g = g
                                self.__frame = 0
                                print("Switch to Goroutine {}".format(goid))
                elif args[0] == "all":
                        if len(args) > 1:
                                self.__exec_all(args[1:])
                elif self.__g is None:
                        print("No Goroutine is selected")
                else:
                        with GoroutineContext(self.__g) as ctxt:
                                if ctxt is None:
                                        pass
                                else:
                                        ctxt.select_frame(self.__frame)
                                        self.__exec(ctxt, args)
                                        self.__frame = gdb.selected_frame().level()

        def __exec(self, ctxt, args):
                try:
                        gdb.execute(" ".join(args))
                except gdb.error as e:
                        print(str(e))

        def __exec_all(self, args):
                for g in Goroutine.all():
                        with GoroutineContext(g) as ctxt:
                                if ctxt is None:
                                        pass
                                else:
                                        g.print()
                                        try:
                                                gdb.execute(" ".join(args))
                                        except gdb.error as e:
                                                print(str(e))

                                        print()


class GoIfaceCmd(gdb.Command):
        "Print Static and dynamic interface types"

        def __init__(self):
                gdb.Command.__init__(self, "iface", gdb.COMMAND_DATA, gdb.COMPLETE_SYMBOL)

        def invoke(self, arg, _from_tty):
                for obj in gdb.string_to_argv(arg):
                        try:
                                #TODO fix quoting for qualified variable names
                                obj = gdb.parse_and_eval(str(obj))
                        except Exception as e:
                                print("Can't parse ", obj, ": ", e)
                                continue

                        if obj['data'] == 0:
                                dtype = "nil"
                        else:
                                dtype = iface_dtype(obj)

                        if dtype is None:
                                print("Not an interface: ", obj.type)
                                continue

                        print("{0}: {1}".format(obj.type, dtype))

# TODO: print interface's methods and dynamic type's func pointers thereof.
#rsc: "to find the number of entries in the itab's Fn field look at
# itab.inter->numMethods
# i am sure i have the names wrong but look at the interface type
# and its method count"
# so Itype will start with a commontype which has kind = interface

#
# Register all convenience functions and CLI commands
#
GoLenFunc()
GoCapFunc()
DTypeFunc()
GoroutinesCmd()
GoroutineCmd()
GoIfaceCmd()
