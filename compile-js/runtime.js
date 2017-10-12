/*
 * (c) Andreas Rossberg 2013
 *
 * Standard ML Basis Library primitives in JavaScript
 *
 * Definition, Sections 6.2, 6.4 and Appendices C, D, and E
 * Standard Basis Specification
 *
 * Note: See lib/DynamicLibrary.sml for additional explanations.
 */

// Save JavaScript intrinsics to protect against overwriting.

var _JS = {
  Object: Object,
  Function: Function,
  Array: Array,
  String: String,
  Number: Number,
  Math: Math,
  Error: Error,
  print: print || console.log || function() { throw "_JS.print not available" }
};


// Initial Dynamic Environment [Appendix D]

function _equal(x, y) { return x === y; }
function _colon_equal(r, x) { r.ref = x; }

var Match = new _JS.Error("Match");
var Bind  = new _JS.Error("Bind");


// Exceptions from the Standard Basis Library

var Chr = new _JS.Error("Chr");
var Div = new _JS.Error("Div");
var Domain = new _JS.Error("Domain");
var Overflow = new _JS.Error("Overflow");
var Size = new _JS.Error("Size");
var Subscript = new _JS.Error("Subscript");

var $IO = {
  Io : function Io(x) {
    if (!(this instanceof Io)) return new Io(x);
    this.Io = x;
  }
};
$IO.Io.prototype = new _JS.Error("Io");

var $OS = {
  SysErr: function SysErr(x) {
    if (!(this instanceof SysErr)) return new SysErr(x);
    this.SysErr = x;
  }
};
$OS.SysErr.prototype = new _JS.Error("SysErr");


// The magic 'use' function

function use(x) {
  if (typeof x === "string") {
    $Use.enqueue(x);
  } else {
    return eval("_SML['" + x.b.replace(/\./g, "']['") + "']");
  }
}


// Library primitives for SML.

var _SML = {
  // Top-level exceptions
  Bind: Bind,
  Match: Match,
  Chr: Chr,
  Div: Div,
  Domain: Domain,
  Overflow: Overflow,
  Size: Size,
  Subscript: Subscript,

  // Internal helpers
  _isExn: function(x) {
    return x instanceof _JS.Error;
  },
  _isTuple: function(x) {
    return x instanceof _JS.Array && !(x instanceof _SML.Vector._constructor);
  },
  _tuplifyArgs: function(args) {
    return args.length <= 1 ? args[0] : [].slice.call(args);
  },
  _checkOverflow: function(i) {
    if (i < -0x80000000 || i >= 0x80000000) throw _SML.Overflow;
    return i;
  },
  _arrayToList: function(a) {
    var l = 'nil';
    for (var i = a.length - 1; i >= 0; --i)
      l = {'::': [a[i], l]};
    return l;
  },

  // Overloaded functions
  abs: {
    real: _JS.Math.abs,
    int: _JS.Math.abs
  },
  '~': {
    real: function(x) { return -x; },
    int: function(x) { return -x; },
    word: function(x) { return -x | 0; },
    word8: function(x) { return -x & 0xff; }
  },
  '+': {
    real: function(x, y) { return x + y; },
    int: function(x, y) { return _SML._checkOverflow(x + y); },
    word: function(x, y) { return (x + y) | 0; },
    word8: function(x, y) { return (x + y) & 0xff; }
  },
  '-': {
    real: function(x, y) { return x - y; },
    int: function(x, y) { return _SML._checkOverflow(x - y); },
    word: function(x, y) { return (x - y) | 0; },
    word8: function(x, y) { return (x - y) & 0xff; }
  },
  '*': {
    real: function(x, y) { return x * y; },
    int: function(x, y) { return _SML._checkOverflow(x * y); },
    word: function(x, y) { return (x * y) | 0; },
    word8: function(x,y ) { return (x * y) & 0xff; }
  },
  div: {
    int: function(x, y) { return _JS.Math.floor(x / y); },
    word: function(x, y) {
      return (_SML.Word._toInt(x) / _SML.Word._toInt(y)) | 0;
    },
    word8: function(x, y) { return (x / y) & 0xff; }
  },
  mod: {
    int: function(x, y) { return x * y >= 0 ? x % y : -(x % y); },
    word: function(x, y) {
      return (_SML.Word._toInt(x) % _SML.Word._toInt(y)) | 0;
    },
    word8: function(x, y) { return (x % y) & 0xff; }
  },
  '/': {
    real: function(x, y) { return x / y; }
  },
  '<': {
    real: function(x, y) { return x < y; },
    int: function(x, y) { return x < y; },
    word: function(x, y) { return _SML.Word._toInt(x) < _SML.Word._toInt(y); },
    word8: function(x, y) { return x < y; },
    char: function(x, y) { return x < y; },
    string: function(x, y) { return x < y; }
  },
  '>': {
    real: function(x, y) { return x > y; },
    int: function(x, y) { return x > y; },
    word: function(x, y) { return _SML.Word._toInt(x) > _SML.Word._toInt(y); },
    word8: function(x, y) { return x > y; },
    char: function(x, y) { return x > y; },
    string: function(x, y) { return x > y; }
  },
  '<=': {
    real: function(x, y) { return x <= y; },
    int: function(x, y) { return x <= y; },
    word: function(x, y) { return _SML.Word._toInt(x) <= _SML.Word._toInt(y); },
    word8: function(x, y) { return x <= y; },
    char: function(x, y) { return x <= y; },
    string: function(x, y) { return x <= y; }
  },
  '>=': {
    real: function(x, y) { return x >= y; },
    int: function(x, y) { return x >= y; },
    word: function(x, y) { return _SML.Word._toInt(x) >= _SML.Word._toInt(y); },
    word8: function(x, y) { return x >= y; },
    char: function(x, y) { return x >= y; },
    string: function(x, y) { return x >= y; }
  },

  // Other library primitives, accessible via the 'use' magic.
  General: {
    exnName: function(e) { return e.message; }
  },

  Char: {
    ord: function(c) { return c.charCodeAt(0); },
    chr: function(i) {
      if (i < 0 || i > 255) throw _SML.Chr;
      return _JS.String.fromCharCode(i);
    }
  },

  String: {
    maxSize: function() { return 0xffffffff; },
    size: function(s) { return s.length; },
    sub: function(s, i) {
      var c = s[i];
      if (c === void 0) throw _SML.Subscript;
      return c;
    },
    str: function(c) { return c; },
    '^': function(s1, s2) { return s1 + s2; }
  },

  Int: {
    precision: function() { return {SOME: 32}; },
    minInt: function() { return {SOME: -0x80000000}; },
    maxInt: function() { return {SOME: 0x7fffffff}; },
    quot: function(x, y) {
      var z = x / y;
      return z >= 0 ? _JS.Math.floor(z) : _JS.Math.ceil(z);
    },
    rem: function(x, y) { return x % y; }
  },

  Word: {
    _toInt: function(w) { return w >= 0 ? w : w + 0x100000000; },
    wordSize: function() { return 32; },
    toInt: function(w) { if (w < 0) throw _SML.Overflow; return w; },
    toIntX: function(w) { return w; },
    fromInt: function(i) { return i | 0; },
    notb: function(w) { return ~w; },
    orb: function(w1, w2) { return w1 | w2; },
    xorb: function(w1, w2) { return w1 ^ w2; },
    andb: function(w1, w2) { return w1 & w2; },
    '<<': function(w1, w2) { return w1 << w2; },
    '>>': function(w1, w2) { return w1 >> w2; },
    '~>>': function(w1, w2) { return w1 >>> w2; }
  },

  Word8: {
    toLarge: function(w) { return w; },
    toLargeX: function(w) { return w < 0x80 ? w : w | 0xffffff00; },
    fromLarge: function(w) { return w & 0xff; },
    toInt: function(w) { return w; },
    toIntX: function(w) { return w < 0x80 ? w : w | 0xffffff00; },
    fromInt: function(i) { return i & 0xff; },
    notb: function(w) { return ~w & 0xff; },
    orb: function(w1, w2) { return w1 | w2; },
    xorb: function(w1, w2) { return w1 ^ w2; },
    andb: function(w1, w2) { return w1 & w2; },
    '<<': function(w1, w2) { return (w1 << w2) & 0xff; },
    '>>': function(w1, w2) { return w1 >> w2; },
    '~>>': function(w1, w2) {
      return ((w1 < 0x80 ? w1 : w1 | 0xffffff00) >>> w2) & 0xff;
    }
  },

  Real: {
    '==': function(r1, r2) { return r1 === r2; },
    '?=': function(r1, r2) { return r1 === r2 || r1 !== r1 || r2 !== r2; },
    isFinite: _JS.Number.isFinite,
    isNan: _JS.Number.isNaN,
    signBit: function(r) { return r < 0 || 1/r < 0; },
    copySign: function(r1, r2) { return r2 < 0 || 1/r2 < 0 ? -r1 : r1; },
    checkFloat: function(r) {
      if (_JS.Number.isNaN(r)) throw _SML.Div;
      if (!_JS.Number.isFinite(r)) throw _SML.Overflow;
      return r;
    },
    floor: function(r) {
      if (r !== r) throw _SML.Domain;
      return _SML._checkOverflow(_JS.Math.floor(r));
    },
    ceil: function(r) {
      if (r !== r) throw _SML.Domain;
      return _SML._checkOverflow(_JS.Math.ceil(r));
    },
    trunc: function(r) {
      if (r !== r) throw _SML.Domain;
      return _SML._checkOverflow(r >= 0 ? _JS.Math.floor(r) : _JS.Math.ceil(r));
    },
    round: function(r) {
      if (r !== r) throw _SML.Domain;
      return _SML._checkOverflow(_JS.Math.round(r));
    },
    fromInt: function(i) { return i; },
    toString: function(r) { return r.toPrecision(12).replace(/-/g, "~"); }
  },

  Math: {
    e: function() { return _JS.Math.E; },
    pi: function() { return _JS.Math.PI; },
    sqrt: _JS.Math.sqrt,
    sin: _JS.Math.sin,
    cos: _JS.Math.cos,
    tan: _JS.Math.tan,
    asin: _JS.Math.asin,
    acos: _JS.Math.acos,
    atan: _JS.Math.atan,
    atan2: function(r1, r2) { return _JS.Math.atan2(r1, r2); },
    exp: _JS.Math.exp,
    pow: function(r1, r2) { return _JS.Math.pow(r1, r2); },
    ln: _JS.Math.log,
    log10: function(r) { return _JS.Math.log(r) / _JS.Math.log(10); },
    sinh: function(r) { return (_JS.Math.exp(r) - _JS.Math.exp(-r)) / 2; },
    cosh: function(r) { return (_JS.Math.exp(r) + _JS.Math.exp(-r)) / 2; },
    tanh: function(r) {
      var r1 = _JS.Math.exp(r), r2 = _JS.Math.exp(-r);
      return (r1 - r2) / (r1 + r2);
    }
  },

  Vector: {
	_constructor: function() {},
    maxLen: function() { return 0xffffffff; },
    fromList: function(l) {
      var v = [];
      v.__proto__ = _SML.Vector._constructor.prototype;
      v.__proto__.__proto__ = _JS.Array.prototype;
      while (l !== 'nil') {
        var cons = l['::'];
        v.push(cons[0]);
        l = cons[1];
      }
      return v;
    },
    length: function(v) { return v.length; },
    sub: function(v, i) {
      var x = v[i];
      if (x === void 0) throw _SML.Subscript;
      return x;
    }
  },

  CharVector: {
    fromList: function(l) {
      var s = [];
      while (l !== 'nil') {
        var cons = l['::'];
        s += cons[0];
        l = cons[1];
      }
      return s;
    }
  },

  IO: {
    _ioErr: function(name, fn, s) {
      var cause = new _SML.OS.SysErr([s, 'NONE']);
      throw new _SML.IO.Io({name: name, function: fn, cause: cause});
    },
    Io: $IO.Io
  },

  TextIO: {
    // Input can be simulated by appending to _SML.TextIO._stdIn.content.
    _stdIn: {content: ""},
    _stdOut: {name: "stdOut", buffer: ""},
    _stdErr: {name: "stdErr", buffer: ""},
    stdIn: function() {
      return {name: "stdIn", file: _SML.TextIO._stdIn, pos: 0};
    },
    stdOut: function() { return _SML.TextIO._stdOut; },
    stdErr: function() { return _SML.TextIO._stdErr; },
    openIn: function(s) {
      var h = _SML.OS.FileSys._findFile(s);
      var i = {name: s, file: h.dir[h.arc], pos: 0};
      if (!i.file)
        _SML.IO._ioErr(s, "openIn", "file does not exist");
      if (!('content' in i.file))
        _SML.IO._ioErr(s, "openIn", "file is a directory");
      return i;
    },
    openOut: function(s, append) {
      var h = _SML.OS.FileSys._findFile(s);
      var o = {name: s, file: h.dir[h.arc], buffer: "", pos: 0};
      if (!o.file)
        h.dir[h.arc] = o.file = {content: ""};
      else if (!('content' in o.file))
        _SML.IO._ioErr(s, "openOut", "file is a directory");
      else if (append)
        o.pos = o.file.content.length;
      else
        o.file.content = "";
      return o;
    },
    openAppend: function(s) { _SML.TextIO.openOut(s, true); },
    closeIn: function(i) { i.pos = 1/0; },
    closeOut: function(o) { _SML.TextIO.flushOut(o); o.pos = 1/0; },
    input: function(i) { return _SML.TextIO.inputAll(i); },
    input1: function(i) { return _SML.TextIO.inputN(i, 1); },
    inputN: function(i, n) {
      if (n < 0) throw _SML.Size;
      var s = i.file.content.slice(i.pos, i.pos + n);
      i.pos += s.length;
      return s;
    },
    inputAll: function(i) {
      var n = i.file.content.length - i.pos;
      if (n <= 0) return "";
      return _SML.TextIO.inputN(i, n);
    },
    inputLine: function(i) {
      var eol = i.file.content.indexOf("\n", i.pos);
      var n = (eol === -1 ? i.file.content.length : eol + 1) - i.pos;
      if (n <= 0) return 'NONE';
      var s = _SML.TextIO.inputN(i, n);
      return {SOME: eol === -1 ? s + "\n" : s};
    },
    endOfStream: function(i) { return i.pos >= i.buffer.length; },
    output: function(o, s) { o.buffer += s; },
    output1: function(o, c) { o.buffer += c; },
    flushOut: function(o) {
      if (o.pos === 1/0) {
        o.buffer = "";
      } else if (o.file) {
        o.file.content = o.file.content.slice(0, o.pos);
        o.file.content += o.buffer;
        o.buffer = "";
        o.pos = o.file.content.length;
      } else {
        // Only flush until last newline, since _JS.print always inserts one.
        var eol = o.buffer.lastIndexOf("\n");
        if (eol >= 0) {
          _JS.print(o.buffer.slice(0, eol));
          o.buffer = o.buffer.slice(eol + 1);
        }
      }
    }
  },

  OS: {
    _sysErr: function(s) { throw new _SML.OS.SysErr([s, 'NONE']); },
    SysErr: $OS.SysErr,
    FileSys: {
      // All arcs are prefixed by "/" to avoid clashes with prototype.
      _root: (function(dir) { return dir["/."] = dir["/.."] = dir; })({}),
      _currentPath: [],
      _path: function(s) {
        var path = s.split("/").filter(function(s) { return s !== ""; })
          .map(function(s) { return "/" + s; });
        return s[0] === "/" ? path : _SML.OS.FileSys._currentPath.concat(path);
      },
      _findFile: function(s) {
        var path = _SML.OS.FileSys._path(s);
        var dir = _SML.OS.FileSys._root;
        for (var i = 0; i < path.length - 1; ++i) {
          var arc = path[i];
          if (!(arc in dir)) _SML.OS._sysErr("file does not exist");
          if ('content' in dir[arc]) _SML.OS._sysErr("file is not a directory");
          dir = dir[arc];
        }
        return {dir: dir, arc: path[path.length - 1]};
      },
      getDir: function(u) {
        return "/" + _SML.OS.FileSys._currentPath.join("/");
      },
      chDir: function(s) {
        _SML.OS.FileSys._currentPath = _SML.OS.FileSys._path(s);
      },
      mkDir: function(s) {
        var path = _SML.OS.FileSys._path(s);
        var dir = _SML.OS.FileSys._root;
        for (var i in path) {
          var arc = path[i];
          if (!(arc in dir)) {
            var newDir = dir[arc] = {};
            newDir["/."] = newDir;
            newDir["/.."] = dir;
          } else if ('content' in dir[arc]) {
            _SML.OS._sysErr("file is not a directory");
          }
          dir = dir[arc];
        }
      },
      rmDir: function(s) {
        var h = _SML.OS.FileSys._findFile(s);
        var dir = h.dir[h.arc];
        if (!dir) _SML.OS._sysErr("directory does not exist");
        if ('content' in dir) _SML.OS._sysErr("file is not a directory");
        if (Objects.keys(dir).length) _SML.OS._sysErr("directory is not empty");
        delete h.dir[h.arc];
      },
      isDir: function(s) {
        var h = _SML.OS.FileSys._findFile(s);
        var dir = h.dir[h.arc];
        if (!dir) _SML.OS._sysErr("file does not exist");
        return !('content' in dir);
      }
    },
    Process: {
      terminate: function(n) {
        _SML.TextIO.flushOut(_SML.TextIO._stdOut);
        _SML.TextIO.flushOut(_SML.TextIO._stdErr);
        throw "OS.Process.terminate(" + n + ")";
      }
    }
  },

  CommandLine: {
    _name: "hamlet",
    _arguments: [],
    name: function() { return _SML.CommandLine._name; },
    arguments: function() {
      return _SML._arrayToList(_SML.CommandLine._arguments);
    },
  },

  // Convenience function for creating virtual files
  file: function(name, s) {
    var o = _SML.TextIO.openOut(name);
    _SML.TextIO.output(o, s);
    _SML.TextIO.closeOut(o);
  }
};
