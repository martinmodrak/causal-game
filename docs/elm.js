(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.al.V === region.at.V)
	{
		return 'on line ' + region.al.V;
	}
	return 'on lines ' + region.al.V + ' through ' + region.at.V;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ba,
		impl.br,
		impl.bn,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		C: func(record.C),
		am: record.am,
		ai: record.ai
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.C;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.am;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.ai) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ba,
		impl.br,
		impl.bn,
		function(sendToApp, initialModel) {
			var view = impl.bu;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ba,
		impl.br,
		impl.bn,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.aj && impl.aj(sendToApp)
			var view = impl.bu;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.aY);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.bo) && (_VirtualDom_doc.title = title = doc.bo);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.bh;
	var onUrlRequest = impl.bi;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		aj: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.aJ === next.aJ
							&& curr.ay === next.ay
							&& curr.aG.a === next.aG.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		ba: function(flags)
		{
			return A3(impl.ba, flags, _Browser_getUrl(), key);
		},
		bu: impl.bu,
		br: impl.br,
		bn: impl.bn
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { a7: 'hidden', aZ: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { a7: 'mozHidden', aZ: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { a7: 'msHidden', aZ: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { a7: 'webkitHidden', aZ: 'webkitvisibilitychange' }
		: { a7: 'hidden', aZ: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		aO: _Browser_getScene(),
		aS: {
			aU: _Browser_window.pageXOffset,
			aV: _Browser_window.pageYOffset,
			aT: _Browser_doc.documentElement.clientWidth,
			ax: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		aT: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		ax: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			aO: {
				aT: node.scrollWidth,
				ax: node.scrollHeight
			},
			aS: {
				aU: node.scrollLeft,
				aV: node.scrollTop,
				aT: node.clientWidth,
				ax: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			aO: _Browser_getScene(),
			aS: {
				aU: x,
				aV: y,
				aT: _Browser_doc.documentElement.clientWidth,
				ax: _Browser_doc.documentElement.clientHeight
			},
			a2: {
				aU: x + rect.left,
				aV: y + rect.top,
				aT: rect.width,
				ax: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.e) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.g),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.g);
		} else {
			var treeLen = builder.e * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.h) : builder.h;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.e);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.g) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.g);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{h: nodeList, e: (len / $elm$core$Array$branchFactor) | 0, g: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {av: fragment, ay: host, aE: path, aG: port_, aJ: protocol, aK: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Main$AssocMsg = function (a) {
	return {$: 0, a: a};
};
var $author$project$Main$AssocPage = 0;
var $author$project$Main$SingleRel = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$TwoRel = function (a) {
	return {$: 2, a: a};
};
var $author$project$Causality$NoCause = 0;
var $author$project$Association$initAdapter = {a$: 100, a0: 0, bb: 4};
var $author$project$Association$costPerExperiment = 2000;
var $author$project$Association$costPerParticipant = 100;
var $author$project$Association$costExperiment = function (exp) {
	return (exp * $author$project$Association$costPerParticipant) + $author$project$Association$costPerExperiment;
};
var $elm_community$intdict$IntDict$foldr = F3(
	function (f, acc, dict) {
		foldr:
		while (true) {
			switch (dict.$) {
				case 0:
					return acc;
				case 1:
					var l = dict.a;
					return A3(f, l.aA, l.z, acc);
				default:
					var i = dict.a;
					var $temp$f = f,
						$temp$acc = A3($elm_community$intdict$IntDict$foldr, f, acc, i.b),
						$temp$dict = i.a;
					f = $temp$f;
					acc = $temp$acc;
					dict = $temp$dict;
					continue foldr;
			}
		}
	});
var $elm_community$intdict$IntDict$toList = function (dict) {
	return A3(
		$elm_community$intdict$IntDict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $author$project$Causality$singleNodeContrib = function (context) {
	var fromId = context.bg.a8;
	var singleEdgeToContrib = function (_v0) {
		var toId = _v0.a;
		var contrib = _v0.b;
		return {af: contrib, a4: fromId, bp: toId};
	};
	var edgeList = $elm_community$intdict$IntDict$toList(context.bk);
	return A2($elm$core$List$map, singleEdgeToContrib, edgeList);
};
var $author$project$Causality$allContrib = function (vars) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Basics$append,
		_List_Nil,
		A2($elm$core$List$map, $author$project$Causality$singleNodeContrib, vars));
};
var $elm_community$intdict$IntDict$Empty = {$: 0};
var $elm_community$intdict$IntDict$empty = $elm_community$intdict$IntDict$Empty;
var $elm_community$intdict$IntDict$Inner = function (a) {
	return {$: 2, a: a};
};
var $elm_community$intdict$IntDict$size = function (dict) {
	switch (dict.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		default:
			var i = dict.a;
			return i.ak;
	}
};
var $elm_community$intdict$IntDict$inner = F3(
	function (p, l, r) {
		var _v0 = _Utils_Tuple2(l, r);
		if (!_v0.a.$) {
			var _v1 = _v0.a;
			return r;
		} else {
			if (!_v0.b.$) {
				var _v2 = _v0.b;
				return l;
			} else {
				return $elm_community$intdict$IntDict$Inner(
					{
						a: l,
						c: p,
						b: r,
						ak: $elm_community$intdict$IntDict$size(l) + $elm_community$intdict$IntDict$size(r)
					});
			}
		}
	});
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$Bitwise$complement = _Bitwise_complement;
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm_community$intdict$IntDict$highestBitSet = function (n) {
	var shiftOr = F2(
		function (i, shift) {
			return i | (i >>> shift);
		});
	var n1 = A2(shiftOr, n, 1);
	var n2 = A2(shiftOr, n1, 2);
	var n3 = A2(shiftOr, n2, 4);
	var n4 = A2(shiftOr, n3, 8);
	var n5 = A2(shiftOr, n4, 16);
	return n5 & (~(n5 >>> 1));
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm_community$intdict$IntDict$signBit = $elm_community$intdict$IntDict$highestBitSet(-1);
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm_community$intdict$IntDict$isBranchingBitSet = F2(
	function (p, i) {
		return !(!(p.m & ($elm_community$intdict$IntDict$signBit ^ i)));
	});
var $elm_community$intdict$IntDict$higherBitMask = function (branchingBit) {
	return branchingBit ^ (~(branchingBit - 1));
};
var $elm_community$intdict$IntDict$lcp = F2(
	function (x, y) {
		var branchingBit = $elm_community$intdict$IntDict$highestBitSet(x ^ y);
		var mask = $elm_community$intdict$IntDict$higherBitMask(branchingBit);
		var prefixBits = x & mask;
		return {m: branchingBit, i: prefixBits};
	});
var $elm_community$intdict$IntDict$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm_community$intdict$IntDict$leaf = F2(
	function (k, v) {
		return $elm_community$intdict$IntDict$Leaf(
			{aA: k, z: v});
	});
var $elm_community$intdict$IntDict$prefixMatches = F2(
	function (p, n) {
		return !((n & $elm_community$intdict$IntDict$higherBitMask(p.m)) - p.i);
	});
var $elm_community$intdict$IntDict$update = F3(
	function (key, alter, dict) {
		var join = F2(
			function (_v2, _v3) {
				var k1 = _v2.a;
				var l = _v2.b;
				var k2 = _v3.a;
				var r = _v3.b;
				var prefix = A2($elm_community$intdict$IntDict$lcp, k1, k2);
				return A2($elm_community$intdict$IntDict$isBranchingBitSet, prefix, k2) ? A3($elm_community$intdict$IntDict$inner, prefix, l, r) : A3($elm_community$intdict$IntDict$inner, prefix, r, l);
			});
		var alteredNode = function (mv) {
			var _v1 = alter(mv);
			if (!_v1.$) {
				var v = _v1.a;
				return A2($elm_community$intdict$IntDict$leaf, key, v);
			} else {
				return $elm_community$intdict$IntDict$empty;
			}
		};
		switch (dict.$) {
			case 0:
				return alteredNode($elm$core$Maybe$Nothing);
			case 1:
				var l = dict.a;
				return (!(l.aA - key)) ? alteredNode(
					$elm$core$Maybe$Just(l.z)) : A2(
					join,
					_Utils_Tuple2(
						key,
						alteredNode($elm$core$Maybe$Nothing)),
					_Utils_Tuple2(l.aA, dict));
			default:
				var i = dict.a;
				return A2($elm_community$intdict$IntDict$prefixMatches, i.c, key) ? (A2($elm_community$intdict$IntDict$isBranchingBitSet, i.c, key) ? A3(
					$elm_community$intdict$IntDict$inner,
					i.c,
					i.a,
					A3($elm_community$intdict$IntDict$update, key, alter, i.b)) : A3(
					$elm_community$intdict$IntDict$inner,
					i.c,
					A3($elm_community$intdict$IntDict$update, key, alter, i.a),
					i.b)) : A2(
					join,
					_Utils_Tuple2(
						key,
						alteredNode($elm$core$Maybe$Nothing)),
					_Utils_Tuple2(i.c.i, dict));
		}
	});
var $elm_community$intdict$IntDict$insert = F3(
	function (key, value, dict) {
		return A3(
			$elm_community$intdict$IntDict$update,
			key,
			$elm$core$Basics$always(
				$elm$core$Maybe$Just(value)),
			dict);
	});
var $elm_community$intdict$IntDict$fromList = function (pairs) {
	return A3(
		$elm$core$List$foldl,
		function (_v0) {
			var a = _v0.a;
			var b = _v0.b;
			return A2($elm_community$intdict$IntDict$insert, a, b);
		},
		$elm_community$intdict$IntDict$empty,
		pairs);
};
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$core$Basics$not = _Basics_not;
var $elm_community$intdict$IntDict$get = F2(
	function (key, dict) {
		get:
		while (true) {
			switch (dict.$) {
				case 0:
					return $elm$core$Maybe$Nothing;
				case 1:
					var l = dict.a;
					return (!(l.aA - key)) ? $elm$core$Maybe$Just(l.z) : $elm$core$Maybe$Nothing;
				default:
					var i = dict.a;
					if (!A2($elm_community$intdict$IntDict$prefixMatches, i.c, key)) {
						return $elm$core$Maybe$Nothing;
					} else {
						if (A2($elm_community$intdict$IntDict$isBranchingBitSet, i.c, key)) {
							var $temp$key = key,
								$temp$dict = i.b;
							key = $temp$key;
							dict = $temp$dict;
							continue get;
						} else {
							var $temp$key = key,
								$temp$dict = i.a;
							key = $temp$key;
							dict = $temp$dict;
							continue get;
						}
					}
			}
		}
	});
var $author$project$Causality$processSingleVar = F3(
	function (intervention, contrib, oldDict) {
		var oldTo = function () {
			var _v1 = A2($elm_community$intdict$IntDict$get, contrib.bp, oldDict);
			if (_v1.$ === 1) {
				return _List_fromArray(
					[0 / 0]);
			} else {
				var x = _v1.a;
				return x;
			}
		}();
		var from = function () {
			var _v0 = A2($elm_community$intdict$IntDict$get, contrib.a4, oldDict);
			if (_v0.$ === 1) {
				return _List_fromArray(
					[0 / 0]);
			} else {
				var x = _v0.a;
				return x;
			}
		}();
		var contribMultiplier = _Utils_eq(intervention, contrib.bp) ? 0.0 : contrib.af;
		var contribValue = A2(
			$elm$core$List$map,
			function (x) {
				return (contribMultiplier * 0.5) * ((x > 0) ? 1 : (-1));
			},
			from);
		var newTo = A3($elm$core$List$map2, $elm$core$Basics$add, oldTo, contribValue);
		return A3($elm_community$intdict$IntDict$insert, contrib.bp, newTo, oldDict);
	});
var $elm_community$intdict$IntDict$values = function (dict) {
	return A3(
		$elm_community$intdict$IntDict$foldr,
		F3(
			function (_v0, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Causality$dataFromNoise = F3(
	function (dag, intervention, probitNoise) {
		var singleInit = F2(
			function (id, _v0) {
				var _var = _v0.a;
				var singleNoise = _v0.b;
				var interceptMult = _Utils_eq(id, intervention) ? 0.0 : 1.0;
				return _Utils_Tuple2(
					id,
					A2(
						$elm$core$List$map,
						function (x) {
							return (interceptMult * _var.az) + x;
						},
						singleNoise));
			});
		var init = $elm_community$intdict$IntDict$fromList(
			A2(
				$elm$core$List$indexedMap,
				singleInit,
				A3($elm$core$List$map2, $elm$core$Tuple$pair, dag.aR, probitNoise)));
		var resultsListFloat = $elm_community$intdict$IntDict$values(
			A3(
				$elm$core$List$foldl,
				$author$project$Causality$processSingleVar(intervention),
				init,
				$author$project$Causality$allContrib(dag.S)));
		return A2(
			$elm$core$List$map,
			$elm$core$List$map(
				function (x) {
					return x > 0;
				}),
			resultsListFloat);
	});
var $elm$random$Random$Generator = $elm$core$Basics$identity;
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0;
		return function (seed0) {
			var _v1 = genA(seed0);
			var a = _v1.a;
			var seed1 = _v1.b;
			return _Utils_Tuple2(
				func(a),
				seed1);
		};
	});
var $author$project$Causality$noIntervention = -1;
var $elm$random$Random$listHelp = F4(
	function (revList, n, gen, seed) {
		listHelp:
		while (true) {
			if (n < 1) {
				return _Utils_Tuple2(revList, seed);
			} else {
				var _v0 = gen(seed);
				var value = _v0.a;
				var newSeed = _v0.b;
				var $temp$revList = A2($elm$core$List$cons, value, revList),
					$temp$n = n - 1,
					$temp$gen = gen,
					$temp$seed = newSeed;
				revList = $temp$revList;
				n = $temp$n;
				gen = $temp$gen;
				seed = $temp$seed;
				continue listHelp;
			}
		}
	});
var $elm$random$Random$list = F2(
	function (n, _v0) {
		var gen = _v0;
		return function (seed) {
			return A4($elm$random$Random$listHelp, _List_Nil, n, gen, seed);
		};
	});
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$e = _Basics_e;
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$float = F2(
	function (a, b) {
		return function (seed0) {
			var seed1 = $elm$random$Random$next(seed0);
			var range = $elm$core$Basics$abs(b - a);
			var n1 = $elm$random$Random$peel(seed1);
			var n0 = $elm$random$Random$peel(seed0);
			var lo = (134217727 & n1) * 1.0;
			var hi = (67108863 & n0) * 1.0;
			var val = ((hi * 134217728.0) + lo) / 9007199254740992.0;
			var scaled = (val * range) + a;
			return _Utils_Tuple2(
				scaled,
				$elm$random$Random$next(seed1));
		};
	});
var $elm$random$Random$map2 = F3(
	function (func, _v0, _v1) {
		var genA = _v0;
		var genB = _v1;
		return function (seed0) {
			var _v2 = genA(seed0);
			var a = _v2.a;
			var seed1 = _v2.b;
			var _v3 = genB(seed1);
			var b = _v3.a;
			var seed2 = _v3.b;
			return _Utils_Tuple2(
				A2(func, a, b),
				seed2);
		};
	});
var $elm$core$Basics$pi = _Basics_pi;
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $elm_community$random_extra$Random$Float$standardNormal = A3(
	$elm$random$Random$map2,
	F2(
		function (u, theta) {
			return $elm$core$Basics$sqrt(
				(-2) * A2(
					$elm$core$Basics$logBase,
					$elm$core$Basics$e,
					1 - A2($elm$core$Basics$max, 0, u))) * $elm$core$Basics$cos(theta);
		}),
	A2($elm$random$Random$float, 0, 1),
	A2($elm$random$Random$float, 0, 2 * $elm$core$Basics$pi));
var $author$project$Causality$probitNoiseGenerator = F2(
	function (nVars, nSubj) {
		return A2(
			$elm$random$Random$list,
			nVars,
			A2($elm$random$Random$list, nSubj, $elm_community$random_extra$Random$Float$standardNormal));
	});
var $author$project$Causality$generatorObservational = F2(
	function (dag, nSubj) {
		return A2(
			$elm$random$Random$map,
			A2($author$project$Causality$dataFromNoise, dag, $author$project$Causality$noIntervention),
			A2(
				$author$project$Causality$probitNoiseGenerator,
				$elm$core$List$length(dag.aR),
				nSubj));
	});
var $author$project$Association$generator = F2(
	function (spec, experiment) {
		return A2($author$project$Causality$generatorObservational, spec.S, experiment);
	});
var $author$project$Causality$causalityDirectionToString = function (dir) {
	switch (dir) {
		case 0:
			return 'is not causally related to';
		case 1:
			return 'promotes';
		case 2:
			return 'inhibits';
		case 3:
			return 'is promoted by';
		default:
			return 'is inhibited by';
	}
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$em = _VirtualDom_node('em');
var $elm$html$Html$strong = _VirtualDom_node('strong');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Causality$causalityDescription = F3(
	function (name1, name2, val) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$em,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(name1)
						])),
					$elm$html$Html$text(' '),
					A2(
					$elm$html$Html$strong,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Causality$causalityDirectionToString(val))
						])),
					$elm$html$Html$text(' '),
					A2(
					$elm$html$Html$em,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(name2)
						]))
				]));
	});
var $author$project$Association$specToNames = function (spec) {
	var _v0 = spec.S.aR;
	if (_v0.b && _v0.b.b) {
		var a = _v0.a;
		var _v1 = _v0.b;
		var b = _v1.a;
		return _Utils_Tuple2(a.aC, b.aC);
	} else {
		return _Utils_Tuple2('Error', 'Error');
	}
};
var $author$project$Association$guessEval = F2(
	function (spec, guess) {
		var _v0 = $author$project$Association$specToNames(spec);
		var name0 = _v0.a;
		var name1 = _v0.b;
		return _Utils_eq(guess, spec.ab) ? _Utils_Tuple2(
			true,
			$elm$html$Html$text('')) : _Utils_Tuple2(
			false,
			A3($author$project$Causality$causalityDescription, name0, name1, spec.ab));
	});
var $elm_community$graph$Graph$Node = F2(
	function (id, label) {
		return {a8: id, bc: label};
	});
var $author$project$Causality$RightNeg = 2;
var $author$project$Causality$RightPos = 1;
var $author$project$Causality$Variable = F2(
	function (name, intercept) {
		return {az: intercept, aC: name};
	});
var $elm$random$Random$andThen = F2(
	function (callback, _v0) {
		var genA = _v0;
		return function (seed) {
			var _v1 = genA(seed);
			var result = _v1.a;
			var newSeed = _v1.b;
			var _v2 = callback(result);
			var genB = _v2;
			return genB(newSeed);
		};
	});
var $elm_community$graph$Graph$AcyclicGraph = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm_community$graph$Graph$Edge = F3(
	function (from, to, label) {
		return {a4: from, bc: label, bp: to};
	});
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (!result.$) {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $elm_community$intdict$IntDict$findMin = function (dict) {
	findMin:
	while (true) {
		switch (dict.$) {
			case 0:
				return $elm$core$Maybe$Nothing;
			case 1:
				var l = dict.a;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(l.aA, l.z));
			default:
				var i = dict.a;
				var $temp$dict = i.a;
				dict = $temp$dict;
				continue findMin;
		}
	}
};
var $elm_community$intdict$IntDict$Disjunct = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm_community$intdict$IntDict$Left = 0;
var $elm_community$intdict$IntDict$Parent = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm_community$intdict$IntDict$Right = 1;
var $elm_community$intdict$IntDict$SamePrefix = {$: 0};
var $elm_community$intdict$IntDict$combineBits = F3(
	function (a, b, mask) {
		return (a & (~mask)) | (b & mask);
	});
var $elm_community$intdict$IntDict$mostSignificantBranchingBit = F2(
	function (a, b) {
		return ((!(a - $elm_community$intdict$IntDict$signBit)) || (!(b - $elm_community$intdict$IntDict$signBit))) ? $elm_community$intdict$IntDict$signBit : A2($elm$core$Basics$max, a, b);
	});
var $elm_community$intdict$IntDict$determineBranchRelation = F2(
	function (l, r) {
		var rp = r.c;
		var lp = l.c;
		if ((!(lp.i - rp.i)) && (!(lp.m - rp.m))) {
			return $elm_community$intdict$IntDict$SamePrefix;
		} else {
			var mask = $elm_community$intdict$IntDict$highestBitSet(
				A2($elm_community$intdict$IntDict$mostSignificantBranchingBit, lp.m, rp.m));
			var modifiedRightPrefix = A3($elm_community$intdict$IntDict$combineBits, rp.i, ~lp.i, mask);
			var prefix = A2($elm_community$intdict$IntDict$lcp, lp.i, modifiedRightPrefix);
			var childEdge = F2(
				function (branchPrefix, c) {
					return A2($elm_community$intdict$IntDict$isBranchingBitSet, branchPrefix, c.c.i) ? 1 : 0;
				});
			return ((!(prefix.i - lp.i)) && (!(prefix.m - lp.m))) ? A2(
				$elm_community$intdict$IntDict$Parent,
				0,
				A2(childEdge, l.c, r)) : (((!(prefix.i - rp.i)) && (!(prefix.m - rp.m))) ? A2(
				$elm_community$intdict$IntDict$Parent,
				1,
				A2(childEdge, r.c, l)) : A2(
				$elm_community$intdict$IntDict$Disjunct,
				prefix,
				A2(childEdge, prefix, l)));
		}
	});
var $elm_community$intdict$IntDict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm_community$intdict$IntDict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm_community$intdict$IntDict$intersect = F2(
	function (l, r) {
		intersect:
		while (true) {
			var _v0 = _Utils_Tuple2(l, r);
			_v0$1:
			while (true) {
				_v0$2:
				while (true) {
					switch (_v0.a.$) {
						case 0:
							var _v1 = _v0.a;
							return $elm_community$intdict$IntDict$Empty;
						case 1:
							switch (_v0.b.$) {
								case 0:
									break _v0$1;
								case 1:
									break _v0$2;
								default:
									break _v0$2;
							}
						default:
							switch (_v0.b.$) {
								case 0:
									break _v0$1;
								case 1:
									var lr = _v0.b.a;
									var _v3 = A2($elm_community$intdict$IntDict$get, lr.aA, l);
									if (!_v3.$) {
										var v = _v3.a;
										return A2($elm_community$intdict$IntDict$leaf, lr.aA, v);
									} else {
										return $elm_community$intdict$IntDict$Empty;
									}
								default:
									var il = _v0.a.a;
									var ir = _v0.b.a;
									var _v4 = A2($elm_community$intdict$IntDict$determineBranchRelation, il, ir);
									switch (_v4.$) {
										case 0:
											return A3(
												$elm_community$intdict$IntDict$inner,
												il.c,
												A2($elm_community$intdict$IntDict$intersect, il.a, ir.a),
												A2($elm_community$intdict$IntDict$intersect, il.b, ir.b));
										case 1:
											if (!_v4.a) {
												if (_v4.b === 1) {
													var _v5 = _v4.a;
													var _v6 = _v4.b;
													var $temp$l = il.b,
														$temp$r = r;
													l = $temp$l;
													r = $temp$r;
													continue intersect;
												} else {
													var _v9 = _v4.a;
													var _v10 = _v4.b;
													var $temp$l = il.a,
														$temp$r = r;
													l = $temp$l;
													r = $temp$r;
													continue intersect;
												}
											} else {
												if (_v4.b === 1) {
													var _v7 = _v4.a;
													var _v8 = _v4.b;
													var $temp$l = l,
														$temp$r = ir.b;
													l = $temp$l;
													r = $temp$r;
													continue intersect;
												} else {
													var _v11 = _v4.a;
													var _v12 = _v4.b;
													var $temp$l = l,
														$temp$r = ir.a;
													l = $temp$l;
													r = $temp$r;
													continue intersect;
												}
											}
										default:
											return $elm_community$intdict$IntDict$Empty;
									}
							}
					}
				}
				var ll = _v0.a.a;
				return A2($elm_community$intdict$IntDict$member, ll.aA, r) ? l : $elm_community$intdict$IntDict$Empty;
			}
			var _v2 = _v0.b;
			return $elm_community$intdict$IntDict$Empty;
		}
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm_community$graph$Graph$crashHack = function (msg) {
	crashHack:
	while (true) {
		var $temp$msg = msg;
		msg = $temp$msg;
		continue crashHack;
	}
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm_community$graph$Graph$unGraph = function (graph) {
	var rep = graph;
	return rep;
};
var $elm_community$graph$Graph$get = function (nodeId) {
	return A2(
		$elm$core$Basics$composeR,
		$elm_community$graph$Graph$unGraph,
		$elm_community$intdict$IntDict$get(nodeId));
};
var $elm_community$graph$Graph$unsafeGet = F3(
	function (msg, id, graph) {
		var _v0 = A2($elm_community$graph$Graph$get, id, graph);
		if (_v0.$ === 1) {
			return $elm_community$graph$Graph$crashHack(msg);
		} else {
			var ctx = _v0.a;
			return ctx;
		}
	});
var $elm_community$graph$Graph$checkForBackEdges = F2(
	function (ordering, graph) {
		var success = function (_v3) {
			return A2($elm_community$graph$Graph$AcyclicGraph, graph, ordering);
		};
		var check = F2(
			function (id, _v2) {
				var backSet = _v2.a;
				var error = 'Graph.checkForBackEdges: `ordering` didn\'t contain `id`';
				var ctx = A3($elm_community$graph$Graph$unsafeGet, error, id, graph);
				var backSetWithId = A3($elm_community$intdict$IntDict$insert, id, 0, backSet);
				var backEdges = A2($elm_community$intdict$IntDict$intersect, ctx.bk, backSetWithId);
				var _v0 = $elm_community$intdict$IntDict$findMin(backEdges);
				if (_v0.$ === 1) {
					return $elm$core$Result$Ok(
						_Utils_Tuple2(backSetWithId, 0));
				} else {
					var _v1 = _v0.a;
					var to = _v1.a;
					var label = _v1.b;
					return $elm$core$Result$Err(
						A3($elm_community$graph$Graph$Edge, id, to, label));
				}
			});
		return A2(
			$elm$core$Result$map,
			success,
			A3(
				$elm$core$List$foldl,
				F2(
					function (id, res) {
						return A2(
							$elm$core$Result$andThen,
							check(id),
							res);
					}),
				$elm$core$Result$Ok(
					_Utils_Tuple2($elm_community$intdict$IntDict$empty, 0)),
				ordering));
	});
var $elm_community$intdict$IntDict$keys = function (dict) {
	return A3(
		$elm_community$intdict$IntDict$foldr,
		F3(
			function (key, _v0, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm_community$graph$Graph$alongOutgoingEdges = function (ctx) {
	return $elm_community$intdict$IntDict$keys(ctx.bk);
};
var $elm_community$graph$Graph$Graph = $elm$core$Basics$identity;
var $elm_community$intdict$IntDict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			switch (dict.$) {
				case 0:
					return acc;
				case 1:
					var l = dict.a;
					return A3(f, l.aA, l.z, acc);
				default:
					var i = dict.a;
					var $temp$f = f,
						$temp$acc = A3($elm_community$intdict$IntDict$foldl, f, acc, i.a),
						$temp$dict = i.b;
					f = $temp$f;
					acc = $temp$acc;
					dict = $temp$dict;
					continue foldl;
			}
		}
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm_community$graph$Graph$applyEdgeDiff = F3(
	function (nodeId, diff, graphRep) {
		var updateOutgoingEdge = F2(
			function (upd, node) {
				return _Utils_update(
					node,
					{
						bk: A3($elm_community$intdict$IntDict$update, nodeId, upd, node.bk)
					});
			});
		var updateIncomingEdge = F2(
			function (upd, node) {
				return _Utils_update(
					node,
					{
						d: A3($elm_community$intdict$IntDict$update, nodeId, upd, node.d)
					});
			});
		var flippedFoldl = F3(
			function (f, dict, acc) {
				return A3($elm_community$intdict$IntDict$foldl, f, acc, dict);
			});
		var edgeUpdateToMaybe = function (edgeUpdate) {
			if (!edgeUpdate.$) {
				var lbl = edgeUpdate.a;
				return $elm$core$Maybe$Just(lbl);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		};
		var updateAdjacency = F3(
			function (updateEdge, updatedId, edgeUpdate) {
				var updateLbl = updateEdge(
					$elm$core$Basics$always(
						edgeUpdateToMaybe(edgeUpdate)));
				return A2(
					$elm_community$intdict$IntDict$update,
					updatedId,
					$elm$core$Maybe$map(updateLbl));
			});
		return A3(
			flippedFoldl,
			updateAdjacency(updateOutgoingEdge),
			diff.bk,
			A3(
				flippedFoldl,
				updateAdjacency(updateIncomingEdge),
				diff.d,
				graphRep));
	});
var $elm_community$graph$Graph$Insert = function (a) {
	return {$: 0, a: a};
};
var $elm_community$graph$Graph$Remove = function (a) {
	return {$: 1, a: a};
};
var $elm_community$graph$Graph$emptyDiff = {d: $elm_community$intdict$IntDict$empty, bk: $elm_community$intdict$IntDict$empty};
var $elm_community$graph$Graph$computeEdgeDiff = F2(
	function (old, _new) {
		var collectUpdates = F3(
			function (edgeUpdate, updatedId, label) {
				var replaceUpdate = function (old_) {
					var _v5 = _Utils_Tuple2(
						old_,
						edgeUpdate(label));
					if (!_v5.a.$) {
						if (_v5.a.a.$ === 1) {
							if (!_v5.b.$) {
								var oldLbl = _v5.a.a.a;
								var newLbl = _v5.b.a;
								return _Utils_eq(oldLbl, newLbl) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
									$elm_community$graph$Graph$Insert(newLbl));
							} else {
								return $elm_community$graph$Graph$crashHack('Graph.computeEdgeDiff: Collected two removals for the same edge. This is an error in the implementation of Graph and you should file a bug report!');
							}
						} else {
							return $elm_community$graph$Graph$crashHack('Graph.computeEdgeDiff: Collected inserts before removals. This is an error in the implementation of Graph and you should file a bug report!');
						}
					} else {
						var _v6 = _v5.a;
						var eu = _v5.b;
						return $elm$core$Maybe$Just(eu);
					}
				};
				return A2($elm_community$intdict$IntDict$update, updatedId, replaceUpdate);
			});
		var collect = F3(
			function (edgeUpdate, adj, updates) {
				return A3(
					$elm_community$intdict$IntDict$foldl,
					collectUpdates(edgeUpdate),
					updates,
					adj);
			});
		var _v0 = _Utils_Tuple2(old, _new);
		if (_v0.a.$ === 1) {
			if (_v0.b.$ === 1) {
				var _v1 = _v0.a;
				var _v2 = _v0.b;
				return $elm_community$graph$Graph$emptyDiff;
			} else {
				var _v4 = _v0.a;
				var ins = _v0.b.a;
				return {
					d: A3(collect, $elm_community$graph$Graph$Insert, ins.bk, $elm_community$intdict$IntDict$empty),
					bk: A3(collect, $elm_community$graph$Graph$Insert, ins.d, $elm_community$intdict$IntDict$empty)
				};
			}
		} else {
			if (_v0.b.$ === 1) {
				var rem = _v0.a.a;
				var _v3 = _v0.b;
				return {
					d: A3(collect, $elm_community$graph$Graph$Remove, rem.bk, $elm_community$intdict$IntDict$empty),
					bk: A3(collect, $elm_community$graph$Graph$Remove, rem.d, $elm_community$intdict$IntDict$empty)
				};
			} else {
				var rem = _v0.a.a;
				var ins = _v0.b.a;
				return _Utils_eq(rem, ins) ? $elm_community$graph$Graph$emptyDiff : {
					d: A3(
						collect,
						$elm_community$graph$Graph$Insert,
						ins.bk,
						A3(collect, $elm_community$graph$Graph$Remove, rem.bk, $elm_community$intdict$IntDict$empty)),
					bk: A3(
						collect,
						$elm_community$graph$Graph$Insert,
						ins.d,
						A3(collect, $elm_community$graph$Graph$Remove, rem.d, $elm_community$intdict$IntDict$empty))
				};
			}
		}
	});
var $elm_community$intdict$IntDict$filter = F2(
	function (predicate, dict) {
		var add = F3(
			function (k, v, d) {
				return A2(predicate, k, v) ? A3($elm_community$intdict$IntDict$insert, k, v, d) : d;
			});
		return A3($elm_community$intdict$IntDict$foldl, add, $elm_community$intdict$IntDict$empty, dict);
	});
var $elm_community$graph$Graph$update = F2(
	function (nodeId, updater) {
		var wrappedUpdater = function (rep) {
			var old = A2($elm_community$intdict$IntDict$get, nodeId, rep);
			var filterInvalidEdges = function (ctx) {
				return $elm_community$intdict$IntDict$filter(
					F2(
						function (id, _v0) {
							return _Utils_eq(id, ctx.bg.a8) || A2($elm_community$intdict$IntDict$member, id, rep);
						}));
			};
			var cleanUpEdges = function (ctx) {
				return _Utils_update(
					ctx,
					{
						d: A2(filterInvalidEdges, ctx, ctx.d),
						bk: A2(filterInvalidEdges, ctx, ctx.bk)
					});
			};
			var _new = A2(
				$elm$core$Maybe$map,
				cleanUpEdges,
				updater(old));
			var diff = A2($elm_community$graph$Graph$computeEdgeDiff, old, _new);
			return A3(
				$elm_community$intdict$IntDict$update,
				nodeId,
				$elm$core$Basics$always(_new),
				A3($elm_community$graph$Graph$applyEdgeDiff, nodeId, diff, rep));
		};
		return A2(
			$elm$core$Basics$composeR,
			$elm_community$graph$Graph$unGraph,
			A2($elm$core$Basics$composeR, wrappedUpdater, $elm$core$Basics$identity));
	});
var $elm_community$graph$Graph$remove = F2(
	function (nodeId, graph) {
		return A3(
			$elm_community$graph$Graph$update,
			nodeId,
			$elm$core$Basics$always($elm$core$Maybe$Nothing),
			graph);
	});
var $elm_community$graph$Graph$guidedDfs = F5(
	function (selectNeighbors, visitNode, startingSeeds, startingAcc, startingGraph) {
		var go = F3(
			function (seeds, acc, graph) {
				go:
				while (true) {
					if (!seeds.b) {
						return _Utils_Tuple2(acc, graph);
					} else {
						var next = seeds.a;
						var seeds1 = seeds.b;
						var _v1 = A2($elm_community$graph$Graph$get, next, graph);
						if (_v1.$ === 1) {
							var $temp$seeds = seeds1,
								$temp$acc = acc,
								$temp$graph = graph;
							seeds = $temp$seeds;
							acc = $temp$acc;
							graph = $temp$graph;
							continue go;
						} else {
							var ctx = _v1.a;
							var _v2 = A2(visitNode, ctx, acc);
							var accAfterDiscovery = _v2.a;
							var finishNode = _v2.b;
							var _v3 = A3(
								go,
								selectNeighbors(ctx),
								accAfterDiscovery,
								A2($elm_community$graph$Graph$remove, next, graph));
							var accBeforeFinish = _v3.a;
							var graph1 = _v3.b;
							var accAfterFinish = finishNode(accBeforeFinish);
							var $temp$seeds = seeds1,
								$temp$acc = accAfterFinish,
								$temp$graph = graph1;
							seeds = $temp$seeds;
							acc = $temp$acc;
							graph = $temp$graph;
							continue go;
						}
					}
				}
			});
		return A3(go, startingSeeds, startingAcc, startingGraph);
	});
var $elm_community$graph$Graph$nodeIds = A2($elm$core$Basics$composeR, $elm_community$graph$Graph$unGraph, $elm_community$intdict$IntDict$keys);
var $elm_community$graph$Graph$dfs = F3(
	function (visitNode, acc, graph) {
		return A5(
			$elm_community$graph$Graph$guidedDfs,
			$elm_community$graph$Graph$alongOutgoingEdges,
			visitNode,
			$elm_community$graph$Graph$nodeIds(graph),
			acc,
			graph).a;
	});
var $elm_community$graph$Graph$onFinish = F3(
	function (visitor, ctx, acc) {
		return _Utils_Tuple2(
			acc,
			visitor(ctx));
	});
var $elm_community$graph$Graph$checkAcyclic = function (graph) {
	var reversePostOrder = A3(
		$elm_community$graph$Graph$dfs,
		$elm_community$graph$Graph$onFinish(
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.bg;
				},
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.a8;
					},
					$elm$core$List$cons))),
		_List_Nil,
		graph);
	return A2($elm_community$graph$Graph$checkForBackEdges, reversePostOrder, graph);
};
var $author$project$Causality$categoryMult = function (cat) {
	switch (cat) {
		case 2:
			return -1;
		case 4:
			return -1;
		case 0:
			return 0;
		default:
			return 1;
	}
};
var $author$project$Causality$contribGenerator = function (cat) {
	var fromVal = function (val) {
		return val * $author$project$Causality$categoryMult(cat);
	};
	return A2(
		$elm$random$Random$map,
		fromVal,
		A2($elm$random$Random$float, 1, 2.5));
};
var $elm_community$graph$Graph$NodeContext = F3(
	function (node, incoming, outgoing) {
		return {d: incoming, bg: node, bk: outgoing};
	});
var $elm_community$graph$Graph$fromNodesAndEdges = F2(
	function (nodes_, edges_) {
		var nodeRep = A3(
			$elm$core$List$foldl,
			function (n) {
				return A2(
					$elm_community$intdict$IntDict$insert,
					n.a8,
					A3($elm_community$graph$Graph$NodeContext, n, $elm_community$intdict$IntDict$empty, $elm_community$intdict$IntDict$empty));
			},
			$elm_community$intdict$IntDict$empty,
			nodes_);
		var addEdge = F2(
			function (edge, rep) {
				var updateOutgoing = function (ctx) {
					return _Utils_update(
						ctx,
						{
							bk: A3($elm_community$intdict$IntDict$insert, edge.bp, edge.bc, ctx.bk)
						});
				};
				var updateIncoming = function (ctx) {
					return _Utils_update(
						ctx,
						{
							d: A3($elm_community$intdict$IntDict$insert, edge.a4, edge.bc, ctx.d)
						});
				};
				return A3(
					$elm_community$intdict$IntDict$update,
					edge.bp,
					$elm$core$Maybe$map(updateIncoming),
					A3(
						$elm_community$intdict$IntDict$update,
						edge.a4,
						$elm$core$Maybe$map(updateOutgoing),
						rep));
			});
		var addEdgeIfValid = F2(
			function (edge, rep) {
				return (A2($elm_community$intdict$IntDict$member, edge.a4, rep) && A2($elm_community$intdict$IntDict$member, edge.bp, rep)) ? A2(addEdge, edge, rep) : rep;
			});
		return A3($elm$core$List$foldl, addEdgeIfValid, nodeRep, edges_);
	});
var $author$project$Causality$interceptGenerator = A2($elm$random$Random$float, -1, 1);
var $elm_community$graph$Graph$topologicalSort = function (_v0) {
	var graph = _v0.a;
	var ordering = _v0.b;
	var error = 'Graph.topologicalSort: Invalid `AcyclicGraph`, where the ordering contained nodes not present in the graph';
	return A2(
		$elm$core$List$map,
		function (id) {
			return A3($elm_community$graph$Graph$unsafeGet, error, id, graph);
		},
		ordering);
};
var $elm$random$Random$addOne = function (value) {
	return _Utils_Tuple2(1, value);
};
var $elm$random$Random$getByWeight = F3(
	function (_v0, others, countdown) {
		getByWeight:
		while (true) {
			var weight = _v0.a;
			var value = _v0.b;
			if (!others.b) {
				return value;
			} else {
				var second = others.a;
				var otherOthers = others.b;
				if (_Utils_cmp(
					countdown,
					$elm$core$Basics$abs(weight)) < 1) {
					return value;
				} else {
					var $temp$_v0 = second,
						$temp$others = otherOthers,
						$temp$countdown = countdown - $elm$core$Basics$abs(weight);
					_v0 = $temp$_v0;
					others = $temp$others;
					countdown = $temp$countdown;
					continue getByWeight;
				}
			}
		}
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $elm$random$Random$weighted = F2(
	function (first, others) {
		var normalize = function (_v0) {
			var weight = _v0.a;
			return $elm$core$Basics$abs(weight);
		};
		var total = normalize(first) + $elm$core$List$sum(
			A2($elm$core$List$map, normalize, others));
		return A2(
			$elm$random$Random$map,
			A2($elm$random$Random$getByWeight, first, others),
			A2($elm$random$Random$float, 0, total));
	});
var $elm$random$Random$uniform = F2(
	function (value, valueList) {
		return A2(
			$elm$random$Random$weighted,
			$elm$random$Random$addOne(value),
			A2($elm$core$List$map, $elm$random$Random$addOne, valueList));
	});
var $author$project$Association$specGenerator = function () {
	var varNames = _List_fromArray(
		['A', 'B']);
	var variables = A2(
		$elm$random$Random$map,
		function (intercepts) {
			return A3($elm$core$List$map2, $author$project$Causality$Variable, varNames, intercepts);
		},
		A2($elm$random$Random$list, 2, $author$project$Causality$interceptGenerator));
	var edgeListFromAssoc = F2(
		function (assocVal, contribVal) {
			if (!assocVal) {
				return _List_Nil;
			} else {
				return _List_fromArray(
					[
						{a4: 0, bc: contribVal, bp: 1}
					]);
			}
		});
	var graphFromCauses = F2(
		function (assocVal, contribVal) {
			return A2(
				$elm_community$graph$Graph$fromNodesAndEdges,
				A2(
					$elm$core$List$map,
					function (x) {
						return A2($elm_community$graph$Graph$Node, x, x);
					},
					_List_fromArray(
						[0, 1])),
				A2(edgeListFromAssoc, assocVal, contribVal));
		});
	var sortedFromCausesAndVars = F3(
		function (assocVal, contribVal, vars) {
			return {
				S: function () {
					var _v1 = $elm_community$graph$Graph$checkAcyclic(
						A2(graphFromCauses, assocVal, contribVal));
					if (!_v1.$) {
						var dag = _v1.a;
						return $elm_community$graph$Graph$topologicalSort(dag);
					} else {
						return _List_Nil;
					}
				}(),
				aR: vars
			};
		});
	var specFromData = F2(
		function (_v0, vars) {
			var assocVal = _v0.a;
			var contribVal = _v0.b;
			return {
				ab: assocVal,
				aq: contribVal,
				S: A3(sortedFromCausesAndVars, assocVal, contribVal, vars)
			};
		});
	var assocContrib = A2(
		$elm$random$Random$andThen,
		function (assocVal) {
			return A2(
				$elm$random$Random$map,
				$elm$core$Tuple$pair(assocVal),
				$author$project$Causality$contribGenerator(assocVal));
		},
		A2(
			$elm$random$Random$uniform,
			0,
			_List_fromArray(
				[1, 2])));
	return A3($elm$random$Random$map2, specFromData, assocContrib, variables);
}();
var $author$project$Causality$maxN = 1000;
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $author$project$Association$updateExperiment = F2(
	function (msg, experiment) {
		var newN = msg;
		var _v1 = $elm$core$String$toInt(newN);
		if (!_v1.$) {
			var n = _v1.a;
			return A2($elm$core$Basics$min, n, $author$project$Causality$maxN);
		} else {
			return experiment;
		}
	});
var $author$project$Association$updateGuess = F2(
	function (msg, _v0) {
		var g = msg;
		return g;
	});
var $author$project$Association$logicAdapter = {a_: $author$project$Association$costExperiment, a5: $author$project$Association$generator, a6: $author$project$Association$guessEval, bm: $author$project$Association$specGenerator, bs: $author$project$Association$updateExperiment, bt: $author$project$Association$updateGuess};
var $author$project$Association$viewCostCommentary = $elm$html$Html$text(
	'Observational study costs CZK ' + ($elm$core$String$fromInt($author$project$Association$costPerExperiment) + (' + CZK ' + ($elm$core$String$fromInt($author$project$Association$costPerParticipant) + ' per participant'))));
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$h4 = _VirtualDom_node('h4');
var $author$project$View$experimentTitle = function (id) {
	return A2(
		$elm$html$Html$h4,
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text(
				'Experiment ' + $elm$core$String$fromInt(id + 1))
			]));
};
var $elm$virtual_dom$VirtualDom$lazy2 = _VirtualDom_lazy2;
var $elm$html$Html$Lazy$lazy2 = $elm$virtual_dom$VirtualDom$lazy2;
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $author$project$Causality$variableNames = function (sorted) {
	return A2(
		$elm$core$List$map,
		function ($) {
			return $.aC;
		},
		sorted.aR);
};
var $gicentre$elm_vegalite$VegaLite$X = 0;
var $gicentre$elm_vegalite$VegaLite$Y = 1;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $gicentre$elm_vegalite$VegaLite$vlPropertyLabel = function (spec) {
	switch (spec) {
		case 0:
			return 'name';
		case 1:
			return 'params';
		case 2:
			return 'description';
		case 3:
			return 'title';
		case 4:
			return 'width';
		case 6:
			return 'width';
		case 5:
			return 'height';
		case 7:
			return 'height';
		case 9:
			return 'padding';
		case 8:
			return 'autosize';
		case 10:
			return 'background';
		case 12:
			return 'usermeta';
		case 11:
			return 'background';
		case 13:
			return 'data';
		case 14:
			return 'datasets';
		case 17:
			return 'projection';
		case 15:
			return 'mark';
		case 16:
			return 'transform';
		case 18:
			return 'encoding';
		case 32:
			return 'config';
		case 33:
			return 'selection';
		case 20:
			return 'concat';
		case 23:
			return 'columns';
		case 21:
			return 'hconcat';
		case 22:
			return 'vconcat';
		case 19:
			return 'layer';
		case 24:
			return 'repeat';
		case 25:
			return 'facet';
		case 28:
			return 'spacing';
		case 29:
			return 'align';
		case 30:
			return 'bounds';
		case 31:
			return 'center';
		case 26:
			return 'spec';
		case 27:
			return 'resolve';
		default:
			return 'view';
	}
};
var $gicentre$elm_vegalite$VegaLite$asSpec = function (specs) {
	return $elm$json$Json$Encode$object(
		A2(
			$elm$core$List$map,
			function (_v0) {
				var s = _v0.a;
				var v = _v0.b;
				return _Utils_Tuple2(
					$gicentre$elm_vegalite$VegaLite$vlPropertyLabel(s),
					v);
			},
			specs));
};
var $gicentre$elm_vegalite$VegaLite$AxTitle = function (a) {
	return {$: 56, a: a};
};
var $gicentre$elm_vegalite$VegaLite$Str = function (a) {
	return {$: 0, a: a};
};
var $gicentre$elm_vegalite$VegaLite$axTitle = function (s) {
	return $gicentre$elm_vegalite$VegaLite$AxTitle(
		$gicentre$elm_vegalite$VegaLite$Str(s));
};
var $gicentre$elm_vegalite$VegaLite$AxcoDomain = function (a) {
	return {$: 3, a: a};
};
var $gicentre$elm_vegalite$VegaLite$Boo = function (a) {
	return {$: 0, a: a};
};
var $gicentre$elm_vegalite$VegaLite$axcoDomain = function (b) {
	return $gicentre$elm_vegalite$VegaLite$AxcoDomain(
		$gicentre$elm_vegalite$VegaLite$Boo(b));
};
var $gicentre$elm_vegalite$VegaLite$AxcoGrid = function (a) {
	return {$: 10, a: a};
};
var $gicentre$elm_vegalite$VegaLite$axcoGrid = function (b) {
	return $gicentre$elm_vegalite$VegaLite$AxcoGrid(
		$gicentre$elm_vegalite$VegaLite$Boo(b));
};
var $gicentre$elm_vegalite$VegaLite$AxcoLabels = function (a) {
	return {$: 17, a: a};
};
var $gicentre$elm_vegalite$VegaLite$axcoLabels = function (b) {
	return $gicentre$elm_vegalite$VegaLite$AxcoLabels(
		$gicentre$elm_vegalite$VegaLite$Boo(b));
};
var $gicentre$elm_vegalite$VegaLite$AxcoTicks = function (a) {
	return {$: 41, a: a};
};
var $gicentre$elm_vegalite$VegaLite$axcoTicks = function (b) {
	return $gicentre$elm_vegalite$VegaLite$AxcoTicks(
		$gicentre$elm_vegalite$VegaLite$Boo(b));
};
var $gicentre$elm_vegalite$VegaLite$Booleans = function (a) {
	return {$: 0, a: a};
};
var $gicentre$elm_vegalite$VegaLite$boos = $gicentre$elm_vegalite$VegaLite$Booleans;
var $gicentre$elm_vegalite$VegaLite$calculateAs = F2(
	function (ex, label) {
		return $elm$core$List$cons(
			_Utils_Tuple2(
				'multiSpecs',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'calculate',
							$elm$json$Json$Encode$string(ex)),
							_Utils_Tuple2(
							'as',
							$elm$json$Json$Encode$string(label))
						]))));
	});
var $gicentre$elm_vegalite$VegaLite$Circle = 6;
var $gicentre$elm_vegalite$VegaLite$VLMark = 15;
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $gicentre$elm_vegalite$VegaLite$markLabel = function (m) {
	switch (m) {
		case 0:
			return 'arc';
		case 1:
			return 'area';
		case 2:
			return 'bar';
		case 3:
			return 'boxplot';
		case 6:
			return 'circle';
		case 4:
			return 'errorband';
		case 5:
			return 'errorbar';
		case 8:
			return 'image';
		case 9:
			return 'line';
		case 7:
			return 'geoshape';
		case 10:
			return 'point';
		case 11:
			return 'rect';
		case 12:
			return 'rule';
		case 13:
			return 'square';
		case 14:
			return 'text';
		case 15:
			return 'tick';
		default:
			return 'trail';
	}
};
var $gicentre$elm_vegalite$VegaLite$ArAria = function (a) {
	return {$: 0, a: a};
};
var $gicentre$elm_vegalite$VegaLite$TTNone = 2;
var $elm$json$Json$Encode$bool = _Json_wrap;
var $gicentre$elm_vegalite$VegaLite$booExpr = F2(
	function (objName, n) {
		if (!n.$) {
			var b = n.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					objName,
					$elm$json$Json$Encode$bool(b))
				]);
		} else {
			var s = n.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					objName,
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(s))
							])))
				]);
		}
	});
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $gicentre$elm_vegalite$VegaLite$strExpr = F2(
	function (objName, s) {
		switch (s.$) {
			case 0:
				var x = s.a;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						objName,
						$elm$json$Json$Encode$string(x))
					]);
			case 1:
				return _List_fromArray(
					[
						_Utils_Tuple2(objName, $elm$json$Json$Encode$null)
					]);
			default:
				var x = s.a;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						objName,
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'expr',
									$elm$json$Json$Encode$string(x))
								])))
					]);
		}
	});
var $gicentre$elm_vegalite$VegaLite$ariaProperty = function (arProp) {
	switch (arProp.$) {
		case 0:
			var b = arProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'aria', b);
		case 1:
			var s = arProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'description', s);
		default:
			var s = arProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'aria',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(s))
							])))
				]);
	}
};
var $gicentre$elm_vegalite$VegaLite$blendModeSpec = function (bm) {
	switch (bm.$) {
		case 0:
			return $elm$json$Json$Encode$null;
		case 1:
			return $elm$json$Json$Encode$string('multiply');
		case 2:
			return $elm$json$Json$Encode$string('screen');
		case 3:
			return $elm$json$Json$Encode$string('overlay');
		case 4:
			return $elm$json$Json$Encode$string('darken');
		case 5:
			return $elm$json$Json$Encode$string('lighten');
		case 6:
			return $elm$json$Json$Encode$string('color-dodge');
		case 7:
			return $elm$json$Json$Encode$string('color-burn');
		case 8:
			return $elm$json$Json$Encode$string('hard-light');
		case 9:
			return $elm$json$Json$Encode$string('soft-light');
		case 10:
			return $elm$json$Json$Encode$string('difference');
		case 11:
			return $elm$json$Json$Encode$string('exclusion');
		case 12:
			return $elm$json$Json$Encode$string('hue');
		case 13:
			return $elm$json$Json$Encode$string('saturation');
		case 14:
			return $elm$json$Json$Encode$string('color');
		case 15:
			return $elm$json$Json$Encode$string('luminosity');
		default:
			var s = bm.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$colorGradientLabel = function (gr) {
	if (!gr) {
		return 'linear';
	} else {
		return 'radial';
	}
};
var $gicentre$elm_vegalite$VegaLite$cursorSpec = function (cur) {
	switch (cur.$) {
		case 0:
			return $elm$json$Json$Encode$string('auto');
		case 1:
			return $elm$json$Json$Encode$string('default');
		case 2:
			return $elm$json$Json$Encode$string('none');
		case 3:
			return $elm$json$Json$Encode$string('context-menu');
		case 4:
			return $elm$json$Json$Encode$string('help');
		case 5:
			return $elm$json$Json$Encode$string('pointer');
		case 6:
			return $elm$json$Json$Encode$string('progress');
		case 7:
			return $elm$json$Json$Encode$string('wait');
		case 8:
			return $elm$json$Json$Encode$string('cell');
		case 9:
			return $elm$json$Json$Encode$string('crosshair');
		case 10:
			return $elm$json$Json$Encode$string('text');
		case 11:
			return $elm$json$Json$Encode$string('vertical-text');
		case 12:
			return $elm$json$Json$Encode$string('alias');
		case 13:
			return $elm$json$Json$Encode$string('copy');
		case 14:
			return $elm$json$Json$Encode$string('move');
		case 15:
			return $elm$json$Json$Encode$string('no-drop');
		case 16:
			return $elm$json$Json$Encode$string('not-allowed');
		case 17:
			return $elm$json$Json$Encode$string('all-scroll');
		case 18:
			return $elm$json$Json$Encode$string('col-resize');
		case 19:
			return $elm$json$Json$Encode$string('row-resize');
		case 20:
			return $elm$json$Json$Encode$string('n-resize');
		case 21:
			return $elm$json$Json$Encode$string('e-resize');
		case 22:
			return $elm$json$Json$Encode$string('s-resize');
		case 23:
			return $elm$json$Json$Encode$string('w-resize');
		case 24:
			return $elm$json$Json$Encode$string('ne-resize');
		case 25:
			return $elm$json$Json$Encode$string('nw-resize');
		case 26:
			return $elm$json$Json$Encode$string('se-resize');
		case 27:
			return $elm$json$Json$Encode$string('sw-resize');
		case 28:
			return $elm$json$Json$Encode$string('ew-resize');
		case 29:
			return $elm$json$Json$Encode$string('ns-resize');
		case 30:
			return $elm$json$Json$Encode$string('nesw-resize');
		case 31:
			return $elm$json$Json$Encode$string('nwse-resize');
		case 32:
			return $elm$json$Json$Encode$string('zoom-in');
		case 33:
			return $elm$json$Json$Encode$string('zoom-out');
		case 34:
			return $elm$json$Json$Encode$string('grab');
		case 35:
			return $elm$json$Json$Encode$string('grabbing');
		default:
			var s = cur.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $elm$json$Json$Encode$float = _Json_wrap;
var $gicentre$elm_vegalite$VegaLite$extentSpec = function (ext) {
	switch (ext.$) {
		case 0:
			return $elm$json$Json$Encode$string('ci');
		case 1:
			return $elm$json$Json$Encode$string('stderr');
		case 2:
			return $elm$json$Json$Encode$string('stdev');
		case 3:
			return $elm$json$Json$Encode$string('iqr');
		case 4:
			return $elm$json$Json$Encode$string('min-max');
		default:
			var sc = ext.a;
			switch (sc.$) {
				case 0:
					var x = sc.a;
					return $elm$json$Json$Encode$float(x);
				case 1:
					return $elm$json$Json$Encode$float(0);
				default:
					var s = sc.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(s))
							]));
			}
	}
};
var $gicentre$elm_vegalite$VegaLite$fontWeightSpec = function (w) {
	switch (w.$) {
		case 3:
			return $elm$json$Json$Encode$string('normal');
		case 0:
			return $elm$json$Json$Encode$string('bold');
		case 1:
			return $elm$json$Json$Encode$string('bolder');
		case 2:
			return $elm$json$Json$Encode$string('lighter');
		case 13:
			var s = w.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
		case 4:
			return $elm$json$Json$Encode$float(100);
		case 5:
			return $elm$json$Json$Encode$float(200);
		case 6:
			return $elm$json$Json$Encode$float(300);
		case 7:
			return $elm$json$Json$Encode$float(400);
		case 8:
			return $elm$json$Json$Encode$float(500);
		case 9:
			return $elm$json$Json$Encode$float(600);
		case 10:
			return $elm$json$Json$Encode$float(700);
		case 11:
			return $elm$json$Json$Encode$float(800);
		default:
			return $elm$json$Json$Encode$float(900);
	}
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $gicentre$elm_vegalite$VegaLite$numExpr = F2(
	function (objName, n) {
		switch (n.$) {
			case 0:
				var x = n.a;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						objName,
						$elm$json$Json$Encode$float(x))
					]);
			case 1:
				return _List_fromArray(
					[
						_Utils_Tuple2(objName, $elm$json$Json$Encode$null)
					]);
			default:
				var s = n.a;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						objName,
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'expr',
									$elm$json$Json$Encode$string(s))
								])))
					]);
		}
	});
var $gicentre$elm_vegalite$VegaLite$stopSpec = function (_v0) {
	var x = _v0.a;
	var c = _v0.b;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'offset',
				$elm$json$Json$Encode$float(x)),
				_Utils_Tuple2(
				'color',
				$elm$json$Json$Encode$string(c))
			]));
};
var $gicentre$elm_vegalite$VegaLite$gradientProperty = function (gp) {
	switch (gp.$) {
		case 0:
			var x = gp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'x1', x);
		case 1:
			var x = gp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'y1', x);
		case 2:
			var x = gp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'x2', x);
		case 3:
			var x = gp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'y2', x);
		case 4:
			var x = gp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'r1', x);
		case 5:
			var x = gp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'r2', x);
		default:
			var grs = gp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'stops',
					A2($elm$json$Json$Encode$list, $gicentre$elm_vegalite$VegaLite$stopSpec, grs))
				]);
	}
};
var $gicentre$elm_vegalite$VegaLite$hAlignSpec = function (al) {
	switch (al.$) {
		case 1:
			return $elm$json$Json$Encode$string('left');
		case 0:
			return $elm$json$Json$Encode$string('center');
		case 2:
			return $elm$json$Json$Encode$string('right');
		default:
			var s = al.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$markInterpolationSpec = function (interp) {
	switch (interp.$) {
		case 7:
			return $elm$json$Json$Encode$string('linear');
		case 8:
			return $elm$json$Json$Encode$string('linear-closed');
		case 12:
			return $elm$json$Json$Encode$string('step');
		case 11:
			return $elm$json$Json$Encode$string('step-before');
		case 10:
			return $elm$json$Json$Encode$string('step-after');
		case 0:
			return $elm$json$Json$Encode$string('basis');
		case 2:
			return $elm$json$Json$Encode$string('basis-open');
		case 1:
			return $elm$json$Json$Encode$string('basis-closed');
		case 4:
			return $elm$json$Json$Encode$string('cardinal');
		case 6:
			return $elm$json$Json$Encode$string('cardinal-open');
		case 5:
			return $elm$json$Json$Encode$string('cardinal-closed');
		case 3:
			return $elm$json$Json$Encode$string('bundle');
		case 9:
			return $elm$json$Json$Encode$string('monotone');
		default:
			var s = interp.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$markOrientationLabel = function (orient) {
	if (!orient) {
		return 'horizontal';
	} else {
		return 'vertical';
	}
};
var $gicentre$elm_vegalite$VegaLite$numsExpr = F2(
	function (objName, ns) {
		if (!ns.$) {
			var xs = ns.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					objName,
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs))
				]);
		} else {
			var s = ns.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					objName,
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(s))
							])))
				]);
		}
	});
var $gicentre$elm_vegalite$VegaLite$multilineTextSpec = function (tText) {
	var _v0 = A2($elm$core$String$split, '\n', tText);
	if (!_v0.b) {
		return $elm$json$Json$Encode$string('');
	} else {
		if (!_v0.b.b) {
			var s = _v0.a;
			return $elm$json$Json$Encode$string(s);
		} else {
			var ss = _v0;
			return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, ss);
		}
	}
};
var $gicentre$elm_vegalite$VegaLite$strExprMultiline = F2(
	function (objName, s) {
		switch (s.$) {
			case 0:
				var x = s.a;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						objName,
						$gicentre$elm_vegalite$VegaLite$multilineTextSpec(x))
					]);
			case 1:
				return _List_fromArray(
					[
						_Utils_Tuple2(objName, $elm$json$Json$Encode$null)
					]);
			default:
				var x = s.a;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						objName,
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'expr',
									$elm$json$Json$Encode$string(x))
								])))
					]);
		}
	});
var $gicentre$elm_vegalite$VegaLite$strokeCapSpec = function (cap) {
	switch (cap.$) {
		case 0:
			return $elm$json$Json$Encode$string('butt');
		case 1:
			return $elm$json$Json$Encode$string('round');
		case 2:
			return $elm$json$Json$Encode$string('square');
		default:
			var s = cap.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$strokeJoinSpec = function (jn) {
	switch (jn.$) {
		case 0:
			return $elm$json$Json$Encode$string('miter');
		case 1:
			return $elm$json$Json$Encode$string('round');
		case 2:
			return $elm$json$Json$Encode$string('bevel');
		default:
			var s = jn.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$symbolLabel = function (sym) {
	switch (sym.$) {
		case 0:
			return 'circle';
		case 1:
			return 'square';
		case 2:
			return 'cross';
		case 3:
			return 'diamond';
		case 4:
			return 'triangle-up';
		case 5:
			return 'triangle-down';
		case 6:
			return 'triangle-left';
		case 7:
			return 'triangle-right';
		case 12:
			return 'triangle';
		case 9:
			return 'stroke';
		case 10:
			return 'arrow';
		case 11:
			return 'wedge';
		case 8:
			var svgPath = sym.a;
			return svgPath;
		default:
			var s = sym.a;
			return s;
	}
};
var $gicentre$elm_vegalite$VegaLite$symbolSpec = function (sym) {
	if (sym.$ === 13) {
		var s = sym.a;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'expr',
					$elm$json$Json$Encode$string(s))
				]));
	} else {
		return $elm$json$Json$Encode$string(
			$gicentre$elm_vegalite$VegaLite$symbolLabel(sym));
	}
};
var $gicentre$elm_vegalite$VegaLite$textDirectionSpec = function (td) {
	switch (td.$) {
		case 0:
			return $elm$json$Json$Encode$string('ltr');
		case 1:
			return $elm$json$Json$Encode$string('rtl');
		default:
			var s = td.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $elm$core$String$trim = _String_trim;
var $gicentre$elm_vegalite$VegaLite$ttContentSpec = function (ttContent) {
	switch (ttContent) {
		case 0:
			return $elm$json$Json$Encode$string('encoding');
		case 1:
			return $elm$json$Json$Encode$string('data');
		default:
			return $elm$json$Json$Encode$string('null');
	}
};
var $gicentre$elm_vegalite$VegaLite$vAlignSpec = function (al) {
	switch (al.$) {
		case 0:
			return $elm$json$Json$Encode$string('top');
		case 1:
			return $elm$json$Json$Encode$string('line-top');
		case 2:
			return $elm$json$Json$Encode$string('middle');
		case 3:
			return $elm$json$Json$Encode$string('bottom');
		case 4:
			return $elm$json$Json$Encode$string('line-bottom');
		case 5:
			return $elm$json$Json$Encode$string('alphabetic');
		default:
			var s = al.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$lineMarkerSpec = function (pm) {
	if (!pm.$) {
		return $elm$json$Json$Encode$bool(false);
	} else {
		var mps = pm.a;
		return $elm$json$Json$Encode$object(
			A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps));
	}
};
var $gicentre$elm_vegalite$VegaLite$markProperty = function (mProp) {
	switch (mProp.$) {
		case 2:
			var aps = mProp.a;
			if (!aps.b) {
				return $gicentre$elm_vegalite$VegaLite$ariaProperty(
					$gicentre$elm_vegalite$VegaLite$ArAria(
						$gicentre$elm_vegalite$VegaLite$Boo(false)));
			} else {
				return A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$ariaProperty, aps);
			}
		case 31:
			var b = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'filled', b);
		case 6:
			var bm = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'blend',
					$gicentre$elm_vegalite$VegaLite$blendModeSpec(bm))
				]);
		case 9:
			var b = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'clip', b);
		case 10:
			var s = mProp.a;
			switch (s.$) {
				case 1:
					return _List_fromArray(
						[
							_Utils_Tuple2('color', $elm$json$Json$Encode$null)
						]);
				case 0:
					var clr = s.a;
					return ($elm$core$String$trim(clr) === '') ? _List_fromArray(
						[
							_Utils_Tuple2('color', $elm$json$Json$Encode$null)
						]) : A2($gicentre$elm_vegalite$VegaLite$strExpr, 'color', s);
				default:
					return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'color', s);
			}
		case 12:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'cornerRadius', n);
		case 13:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'cornerRadiusEnd', n);
		case 16:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'cornerRadiusBottomLeft', n);
		case 17:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'cornerRadiusBottomRight', n);
		case 14:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'cornerRadiusTopLeft', n);
		case 15:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'cornerRadiusTopRight', n);
		case 18:
			var cur = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'cursor',
					$gicentre$elm_vegalite$VegaLite$cursorSpec(cur))
				]);
		case 28:
			var ext = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'extent',
					$gicentre$elm_vegalite$VegaLite$extentSpec(ext))
				]);
		case 19:
			var s = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'href', s);
		case 50:
			var bl = mProp.a;
			if (!bl.$) {
				var b = bl.a;
				return b ? _List_fromArray(
					[
						_Utils_Tuple2(
						'invalid',
						$elm$json$Json$Encode$string('filter'))
					]) : _List_fromArray(
					[
						_Utils_Tuple2('invalid', $elm$json$Json$Encode$null)
					]);
			} else {
				return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'invalid', bl);
			}
		case 29:
			var s = mProp.a;
			switch (s.$) {
				case 1:
					return _List_fromArray(
						[
							_Utils_Tuple2('fill', $elm$json$Json$Encode$null)
						]);
				case 0:
					var clr = s.a;
					return ($elm$core$String$trim(clr) === '') ? _List_fromArray(
						[
							_Utils_Tuple2('fill', $elm$json$Json$Encode$null)
						]) : A2($gicentre$elm_vegalite$VegaLite$strExpr, 'fill', s);
				default:
					return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'fill', s);
			}
		case 30:
			var cGrad = mProp.a;
			var props = mProp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'fill',
					$elm$json$Json$Encode$object(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								'gradient',
								$elm$json$Json$Encode$string(
									$gicentre$elm_vegalite$VegaLite$colorGradientLabel(cGrad))),
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$gradientProperty, props))))
				]);
		case 11:
			var cGrad = mProp.a;
			var props = mProp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'color',
					$elm$json$Json$Encode$object(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								'gradient',
								$elm$json$Json$Encode$string(
									$gicentre$elm_vegalite$VegaLite$colorGradientLabel(cGrad))),
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$gradientProperty, props))))
				]);
		case 56:
			var cGrad = mProp.a;
			var props = mProp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'stroke',
					$elm$json$Json$Encode$object(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								'gradient',
								$elm$json$Json$Encode$string(
									$gicentre$elm_vegalite$VegaLite$colorGradientLabel(cGrad))),
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$gradientProperty, props))))
				]);
		case 55:
			var s = mProp.a;
			switch (s.$) {
				case 1:
					return _List_fromArray(
						[
							_Utils_Tuple2('stroke', $elm$json$Json$Encode$null)
						]);
				case 0:
					var clr = s.a;
					return ($elm$core$String$trim(clr) === '') ? _List_fromArray(
						[
							_Utils_Tuple2('stroke', $elm$json$Json$Encode$null)
						]) : A2($gicentre$elm_vegalite$VegaLite$strExpr, 'stroke', s);
				default:
					return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'stroke', s);
			}
		case 57:
			var sc = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'strokeCap',
					$gicentre$elm_vegalite$VegaLite$strokeCapSpec(sc))
				]);
		case 60:
			var sj = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'strokeJoin',
					$gicentre$elm_vegalite$VegaLite$strokeJoinSpec(sj))
				]);
		case 61:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeMiterLimit', n);
		case 42:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'opacity', n);
		case 32:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'fillOpacity', n);
		case 62:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeOpacity', n);
		case 63:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeWidth', n);
		case 58:
			var ns = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'strokeDash', ns);
		case 59:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeDashOffset', n);
		case 64:
			var styles = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'style',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, styles))
				]);
		case 38:
			var interp = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'interpolate',
					$gicentre$elm_vegalite$VegaLite$markInterpolationSpec(interp))
				]);
		case 65:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tension', n);
		case 46:
			var orient = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'orient',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$markOrientationLabel(orient)))
				]);
		case 52:
			var sym = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'shape',
					$gicentre$elm_vegalite$VegaLite$symbolSpec(sym))
				]);
		case 54:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'size', n);
		case 1:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'angle', n);
		case 0:
			var al = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'align',
					$gicentre$elm_vegalite$VegaLite$hAlignSpec(al))
				]);
		case 4:
			var va = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'baseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 26:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'dx', n);
		case 27:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'dy', n);
		case 33:
			var s = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'font', s);
		case 34:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'fontSize', n);
		case 35:
			var s = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'fontStyle', s);
		case 36:
			var w = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'fontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(w))
				]);
		case 49:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'radius', n);
		case 37:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'innerRadius', n);
		case 43:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'outerRadius', n);
		case 47:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'padAngle', n);
		case 66:
			var s = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExprMultiline, 'text', s);
		case 40:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'lineHeight', n);
		case 22:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'limit', n);
		case 23:
			var s = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'ellipsis', s);
		case 24:
			var td = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'dir',
					$gicentre$elm_vegalite$VegaLite$textDirectionSpec(td))
				]);
		case 67:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'theta', n);
		case 68:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'theta2', n);
		case 85:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'thetaOffset', n);
		case 86:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'theta2Offset', n);
		case 5:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'binSpacing', n);
		case 20:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'continuousBandSize', n);
		case 21:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'minBandSize', n);
		case 25:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'discreteBandSize', n);
		case 53:
			var b = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'shortTimeLabels', b);
		case 3:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'bandSize', n);
		case 69:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'thickness', n);
		case 51:
			var props = mProp.a;
			if (!props.b) {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'rule',
						$elm$json$Json$Encode$bool(false))
					]);
			} else {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'rule',
						$elm$json$Json$Encode$object(
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, props)))
					]);
			}
		case 7:
			var props = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'borders',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, props)))
				]);
		case 41:
			var props = mProp.a;
			if (!props.b) {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'median',
						$elm$json$Json$Encode$bool(false))
					]);
			} else {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'median',
						$elm$json$Json$Encode$object(
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, props)))
					]);
			}
		case 8:
			var props = mProp.a;
			if (!props.b) {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'box',
						$elm$json$Json$Encode$bool(false))
					]);
			} else {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'box',
						$elm$json$Json$Encode$object(
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, props)))
					]);
			}
		case 44:
			var props = mProp.a;
			if (!props.b) {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'outliers',
						$elm$json$Json$Encode$bool(false))
					]);
			} else {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'outliers',
						$elm$json$Json$Encode$object(
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, props)))
					]);
			}
		case 70:
			var props = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'ticks',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, props)))
				]);
		case 71:
			var ttContent = mProp.a;
			return (ttContent === 2) ? _List_fromArray(
				[
					_Utils_Tuple2('tooltip', $elm$json$Json$Encode$null)
				]) : _List_fromArray(
				[
					_Utils_Tuple2(
					'tooltip',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'content',
								$gicentre$elm_vegalite$VegaLite$ttContentSpec(ttContent))
							])))
				]);
		case 48:
			var pm = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'point',
					$gicentre$elm_vegalite$VegaLite$pointMarkerSpec(pm))
				]);
		case 39:
			var lm = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'line',
					$gicentre$elm_vegalite$VegaLite$lineMarkerSpec(lm))
				]);
		case 73:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'width', n);
		case 74:
			var n = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'width',
					$elm$json$Json$Encode$object(
						A2($gicentre$elm_vegalite$VegaLite$numExpr, 'band', n)))
				]);
		case 75:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'height', n);
		case 76:
			var n = mProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'height',
					$elm$json$Json$Encode$object(
						A2($gicentre$elm_vegalite$VegaLite$numExpr, 'band', n)))
				]);
		case 77:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'x', n);
		case 78:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'y', n);
		case 79:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'x2', n);
		case 80:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'y2', n);
		case 45:
			var b = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'order', b);
		case 81:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'xOffset', n);
		case 83:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'x2Offset', n);
		case 82:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'yOffset', n);
		case 84:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'y2Offset', n);
		case 87:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'radiusOffset', n);
		case 88:
			var n = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'radius2Offset', n);
		case 89:
			var b = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'aspect', b);
		default:
			var s = mProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'url', s);
	}
};
var $gicentre$elm_vegalite$VegaLite$pointMarkerSpec = function (pm) {
	switch (pm.$) {
		case 0:
			return $elm$json$Json$Encode$string('transparent');
		case 1:
			return $elm$json$Json$Encode$bool(false);
		default:
			var mps = pm.a;
			return _Utils_eq(mps, _List_Nil) ? $elm$json$Json$Encode$bool(true) : $elm$json$Json$Encode$object(
				A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps));
	}
};
var $gicentre$elm_vegalite$VegaLite$mark = F2(
	function (m, mProps) {
		if (!mProps.b) {
			return _Utils_Tuple2(
				15,
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$markLabel(m)));
		} else {
			return _Utils_Tuple2(
				15,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string(
								$gicentre$elm_vegalite$VegaLite$markLabel(m))),
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mProps))));
		}
	});
var $gicentre$elm_vegalite$VegaLite$circle = $gicentre$elm_vegalite$VegaLite$mark(6);
var $gicentre$elm_vegalite$VegaLite$AxBoth = 0;
var $gicentre$elm_vegalite$VegaLite$Axis = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $gicentre$elm_vegalite$VegaLite$coAxis = $gicentre$elm_vegalite$VegaLite$Axis(0);
var $gicentre$elm_vegalite$VegaLite$arrangementLabel = function (arrng) {
	switch (arrng) {
		case 1:
			return 'row';
		case 0:
			return 'column';
		case 2:
			return 'repeat';
		default:
			return 'layer';
	}
};
var $gicentre$elm_vegalite$VegaLite$binProperty = function (binProp) {
	switch (binProp.$) {
		case 5:
			var x = binProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'maxbins', x);
		case 0:
			var x = binProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'anchor', x);
		case 1:
			var x = binProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'base', x);
		case 8:
			var x = binProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'step', x);
		case 9:
			var xs = binProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'steps', xs);
		case 6:
			var x = binProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'minstep', x);
		case 2:
			var xs = binProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'divide', xs);
		case 3:
			var ns = binProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'extent', ns);
		case 4:
			var se = binProp.a;
			switch (se.$) {
				case 0:
					var s = se.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'extent',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'param',
										$elm$json$Json$Encode$string(s))
									])))
						]);
				case 2:
					var s = se.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'extent',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'param',
										$elm$json$Json$Encode$string(s))
									])))
						]);
				default:
					return _List_fromArray(
						[
							_Utils_Tuple2('extent', $elm$json$Json$Encode$null)
						]);
			}
		default:
			var b = binProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'nice', b);
	}
};
var $gicentre$elm_vegalite$VegaLite$bin = function (bProps) {
	return _Utils_eq(bProps, _List_Nil) ? _Utils_Tuple2(
		'bin',
		$elm$json$Json$Encode$bool(true)) : _Utils_Tuple2(
		'bin',
		$elm$json$Json$Encode$object(
			A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$binProperty, bProps)));
};
var $gicentre$elm_vegalite$VegaLite$dayLabel = function (dayName) {
	switch (dayName) {
		case 0:
			return 'Mon';
		case 1:
			return 'Tue';
		case 2:
			return 'Wed';
		case 3:
			return 'Thu';
		case 4:
			return 'Fri';
		case 5:
			return 'Sat';
		default:
			return 'Sun';
	}
};
var $gicentre$elm_vegalite$VegaLite$monthNameLabel = function (mon) {
	switch (mon) {
		case 0:
			return 'Jan';
		case 1:
			return 'Feb';
		case 2:
			return 'Mar';
		case 3:
			return 'Apr';
		case 4:
			return 'May';
		case 5:
			return 'Jun';
		case 6:
			return 'Jul';
		case 7:
			return 'Aug';
		case 8:
			return 'Sep';
		case 9:
			return 'Oct';
		case 10:
			return 'Nov';
		default:
			return 'Dec';
	}
};
var $gicentre$elm_vegalite$VegaLite$dateTimeProperty = function (dtp) {
	switch (dtp.$) {
		case 0:
			var x = dtp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'year', x);
		case 1:
			var x = dtp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'quarter', x);
		case 2:
			var mon = dtp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'month',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$monthNameLabel(mon)))
				]);
		case 3:
			var x = dtp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'month', x);
		case 4:
			var x = dtp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'date', x);
		case 5:
			var d = dtp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'day',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$dayLabel(d)))
				]);
		case 6:
			var x = dtp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'hours', x);
		case 7:
			var x = dtp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'minutes', x);
		case 8:
			var x = dtp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'seconds', x);
		default:
			var x = dtp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'milliseconds', x);
	}
};
var $gicentre$elm_vegalite$VegaLite$toList = $elm$json$Json$Encode$list($elm$core$Basics$identity);
var $gicentre$elm_vegalite$VegaLite$dataValueSpec = function (val) {
	switch (val.$) {
		case 2:
			var x = val.a;
			return $elm$json$Json$Encode$float(x);
		case 3:
			var s = val.a;
			return $elm$json$Json$Encode$string(s);
		case 0:
			var b = val.a;
			return $elm$json$Json$Encode$bool(b);
		case 1:
			var d = val.a;
			return $elm$json$Json$Encode$object(
				A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d));
		case 4:
			var s = val.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
		case 5:
			return $elm$json$Json$Encode$null;
		case 6:
			var vals = val.a;
			return $gicentre$elm_vegalite$VegaLite$dataValuesSpecs(vals);
		case 7:
			var kvs = val.a;
			return $elm$json$Json$Encode$object(
				A2(
					$elm$core$List$map,
					function (_v3) {
						var k = _v3.a;
						var v = _v3.b;
						return _Utils_Tuple2(
							k,
							$gicentre$elm_vegalite$VegaLite$dataValueSpec(v));
					},
					kvs));
		default:
			var xs = val.a;
			return A2($elm$json$Json$Encode$list, $gicentre$elm_vegalite$VegaLite$dataValueSpec, xs);
	}
};
var $gicentre$elm_vegalite$VegaLite$dataValuesSpecs = function (dvs) {
	switch (dvs.$) {
		case 2:
			var xs = dvs.a;
			return $gicentre$elm_vegalite$VegaLite$toList(
				A2($elm$core$List$map, $elm$json$Json$Encode$float, xs));
		case 4:
			var ss = dvs.a;
			return $gicentre$elm_vegalite$VegaLite$toList(
				A2($elm$core$List$map, $elm$json$Json$Encode$string, ss));
		case 1:
			var dtss = dvs.a;
			return $gicentre$elm_vegalite$VegaLite$toList(
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$List$concatMap($gicentre$elm_vegalite$VegaLite$dateTimeProperty),
						$elm$json$Json$Encode$object),
					dtss));
		case 3:
			var s = dvs.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
		case 0:
			var bs = dvs.a;
			return $gicentre$elm_vegalite$VegaLite$toList(
				A2($elm$core$List$map, $elm$json$Json$Encode$bool, bs));
		case 5:
			var obs = dvs.a;
			return $gicentre$elm_vegalite$VegaLite$toList(
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$List$map(
							function (_v1) {
								var k = _v1.a;
								var v = _v1.b;
								return _Utils_Tuple2(
									k,
									$gicentre$elm_vegalite$VegaLite$dataValueSpec(v));
							}),
						$elm$json$Json$Encode$object),
					obs));
		default:
			var ds = dvs.a;
			return $gicentre$elm_vegalite$VegaLite$toList(
				A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dataValuesSpecs, ds));
	}
};
var $gicentre$elm_vegalite$VegaLite$filterProperties = function (f) {
	switch (f.$) {
		case 0:
			var field = f.a;
			var val = f.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(field)),
					_Utils_Tuple2(
					'equal',
					$gicentre$elm_vegalite$VegaLite$dataValueSpec(val))
				]);
		case 1:
			var field = f.a;
			var val = f.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(field)),
					_Utils_Tuple2(
					'lt',
					$gicentre$elm_vegalite$VegaLite$dataValueSpec(val))
				]);
		case 2:
			var field = f.a;
			var val = f.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(field)),
					_Utils_Tuple2(
					'lte',
					$gicentre$elm_vegalite$VegaLite$dataValueSpec(val))
				]);
		case 3:
			var field = f.a;
			var val = f.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(field)),
					_Utils_Tuple2(
					'gt',
					$gicentre$elm_vegalite$VegaLite$dataValueSpec(val))
				]);
		case 4:
			var field = f.a;
			var val = f.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(field)),
					_Utils_Tuple2(
					'gte',
					$gicentre$elm_vegalite$VegaLite$dataValueSpec(val))
				]);
		case 7:
			var selName = f.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'param',
					$elm$json$Json$Encode$string(selName))
				]);
		case 8:
			var selName = f.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'param',
					$elm$json$Json$Encode$string(selName)),
					_Utils_Tuple2(
					'empty',
					$elm$json$Json$Encode$bool(false))
				]);
		case 10:
			var field = f.a;
			var vals = f.b;
			var fromTs = function (ts) {
				if (ts.$ === 1) {
					var s = ts.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(s))
							]));
				} else {
					if (!ts.a.b) {
						return $elm$json$Json$Encode$null;
					} else {
						var d = ts.a;
						return $elm$json$Json$Encode$object(
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d));
					}
				}
			};
			var values = function () {
				if (!vals.$) {
					var ns = vals.a;
					if (!ns.$) {
						var xs = ns.a;
						if ((xs.b && xs.b.b) && (!xs.b.b.b)) {
							var mn = xs.a;
							var _v4 = xs.b;
							var mx = _v4.a;
							return A2(
								$elm$json$Json$Encode$list,
								$elm$json$Json$Encode$float,
								_List_fromArray(
									[mn, mx]));
						} else {
							return $elm$json$Json$Encode$null;
						}
					} else {
						var s = ns.a;
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'expr',
									$elm$json$Json$Encode$string(s))
								]));
					}
				} else {
					var ts1 = vals.a;
					var ts2 = vals.b;
					return $gicentre$elm_vegalite$VegaLite$toList(
						_List_fromArray(
							[
								fromTs(ts1),
								fromTs(ts2)
							]));
				}
			}();
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(field)),
					_Utils_Tuple2('range', values)
				]);
		case 9:
			var field = f.a;
			var vals = f.b;
			var values = function () {
				switch (vals.$) {
					case 2:
						var xs = vals.a;
						return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs);
					case 1:
						var ds = vals.a;
						return A2(
							$elm$json$Json$Encode$list,
							function (d) {
								return $elm$json$Json$Encode$object(
									A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d));
							},
							ds);
					case 4:
						var ss = vals.a;
						return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, ss);
					case 3:
						var s = vals.a;
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'expr',
									$elm$json$Json$Encode$string(s))
								]));
					case 0:
						var bs = vals.a;
						return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$bool, bs);
					case 5:
						var obs = vals.a;
						return A2(
							$elm$json$Json$Encode$list,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$List$map(
									function (_v7) {
										var k = _v7.a;
										var v = _v7.b;
										return _Utils_Tuple2(
											k,
											$gicentre$elm_vegalite$VegaLite$dataValueSpec(v));
									}),
								$elm$json$Json$Encode$object),
							obs);
					default:
						var ds = vals.a;
						return A2($elm$json$Json$Encode$list, $gicentre$elm_vegalite$VegaLite$dataValuesSpecs, ds);
				}
			}();
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(field)),
					_Utils_Tuple2('oneOf', values)
				]);
		case 11:
			var field = f.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(field)),
					_Utils_Tuple2(
					'valid',
					$elm$json$Json$Encode$bool(true))
				]);
		default:
			return _List_Nil;
	}
};
var $gicentre$elm_vegalite$VegaLite$anchorSpec = function (an) {
	switch (an.$) {
		case 0:
			return $elm$json$Json$Encode$string('start');
		case 1:
			return $elm$json$Json$Encode$string('middle');
		case 2:
			return $elm$json$Json$Encode$string('end');
		default:
			var s = an.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$compositionAlignmentLabel = function (ca) {
	switch (ca) {
		case 0:
			return 'none';
		case 1:
			return 'each';
		default:
			return 'all';
	}
};
var $gicentre$elm_vegalite$VegaLite$legendOrientSpec = function (orient) {
	switch (orient) {
		case 3:
			return $elm$json$Json$Encode$string('left');
		case 7:
			return $elm$json$Json$Encode$string('top-left');
		case 6:
			return $elm$json$Json$Encode$string('top');
		case 8:
			return $elm$json$Json$Encode$string('top-right');
		case 5:
			return $elm$json$Json$Encode$string('right');
		case 2:
			return $elm$json$Json$Encode$string('bottom-right');
		case 0:
			return $elm$json$Json$Encode$string('bottom');
		case 1:
			return $elm$json$Json$Encode$string('bottom-left');
		default:
			return $elm$json$Json$Encode$string('none');
	}
};
var $gicentre$elm_vegalite$VegaLite$overlapStrategySpec = function (strat) {
	switch (strat.$) {
		case 0:
			return $elm$json$Json$Encode$bool(false);
		case 1:
			return $elm$json$Json$Encode$string('parity');
		case 2:
			return $elm$json$Json$Encode$string('greedy');
		default:
			var s = strat.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$legendProperty = function (legendProp) {
	switch (legendProp.$) {
		case 0:
			var aps = legendProp.a;
			if (!aps.b) {
				return $gicentre$elm_vegalite$VegaLite$ariaProperty(
					$gicentre$elm_vegalite$VegaLite$ArAria(
						$gicentre$elm_vegalite$VegaLite$Boo(false)));
			} else {
				return A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$ariaProperty, aps);
			}
		case 1:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'clipHeight', n);
		case 2:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'columnPadding', n);
		case 32:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'rowPadding', n);
		case 3:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'columns', n);
		case 4:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'cornerRadius', n);
		case 6:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'fillColor', s);
		case 5:
			var d = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'direction',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$markOrientationLabel(d)))
				]);
		case 59:
			var lType = legendProp.a;
			if (!lType) {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('gradient'))
					]);
			} else {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('symbol'))
					]);
			}
		case 7:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'format', s);
		case 8:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'formatType',
					$elm$json$Json$Encode$string('number'))
				]);
		case 9:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'formatType',
					$elm$json$Json$Encode$string('time'))
				]);
		case 10:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'formatType', s);
		case 11:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientLength', n);
		case 12:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientOpacity', n);
		case 13:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientThickness', n);
		case 14:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'gradientStrokeColor', s);
		case 15:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientStrokeWidth', n);
		case 16:
			var ga = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'gridAlign',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$compositionAlignmentLabel(ga)))
				]);
		case 17:
			var ha = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelAlign',
					$gicentre$elm_vegalite$VegaLite$hAlignSpec(ha))
				]);
		case 18:
			var va = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelBaseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 19:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelColor', s);
		case 20:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelExpr', s);
		case 21:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelFont', s);
		case 22:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelFontSize', n);
		case 23:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelFontStyle', s);
		case 24:
			var fw = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelFontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw))
				]);
		case 25:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelLimit', n);
		case 26:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelOffset', n);
		case 27:
			var lo = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelOverlap',
					$gicentre$elm_vegalite$VegaLite$overlapStrategySpec(lo))
				]);
		case 28:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'offset', n);
		case 29:
			var orient = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'orient',
					$gicentre$elm_vegalite$VegaLite$legendOrientSpec(orient))
				]);
		case 31:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'padding', n);
		case 33:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'strokeColor', s);
		case 34:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeWidth', n);
		case 35:
			var sd = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'symbolDash', sd);
		case 36:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolDashOffset', n);
		case 37:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'symbolFillColor', s);
		case 38:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolLimit', n);
		case 39:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolOffset', n);
		case 40:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolOpacity', n);
		case 43:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'symbolStrokeColor', s);
		case 44:
			var s = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'symbolType',
					$gicentre$elm_vegalite$VegaLite$symbolSpec(s))
				]);
		case 41:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolSize', n);
		case 42:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolStrokeWidth', n);
		case 45:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickCount', n);
		case 46:
			var txt = legendProp.a;
			switch (txt.$) {
				case 1:
					return _List_fromArray(
						[
							_Utils_Tuple2('title', $elm$json$Json$Encode$null)
						]);
				case 0:
					var s = txt.a;
					return (s === '') ? _List_fromArray(
						[
							_Utils_Tuple2('title', $elm$json$Json$Encode$null)
						]) : _List_fromArray(
						[
							_Utils_Tuple2(
							'title',
							$gicentre$elm_vegalite$VegaLite$multilineTextSpec(s))
						]);
				default:
					return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'title', txt);
			}
		case 47:
			var ha = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleAlign',
					$gicentre$elm_vegalite$VegaLite$hAlignSpec(ha))
				]);
		case 48:
			var an = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleAnchor',
					$gicentre$elm_vegalite$VegaLite$anchorSpec(an))
				]);
		case 49:
			var va = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleBaseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 50:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleColor', s);
		case 51:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleFont', s);
		case 52:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleFontSize', n);
		case 53:
			var s = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleFontStyle', s);
		case 54:
			var fw = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleFontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw))
				]);
		case 55:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleLimit', n);
		case 56:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleLineHeight', n);
		case 57:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleOpacity', n);
		case 30:
			var orient = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleOrient',
					$gicentre$elm_vegalite$VegaLite$legendOrientSpec(orient))
				]);
		case 58:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titlePadding', n);
		case 60:
			var vals = legendProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'values',
					$gicentre$elm_vegalite$VegaLite$dataValuesSpecs(vals))
				]);
		case 61:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'legendX', n);
		case 62:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'legendY', n);
		default:
			var n = legendProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'zindex', n);
	}
};
var $gicentre$elm_vegalite$VegaLite$measurementLabel = function (mType) {
	switch (mType) {
		case 0:
			return 'nominal';
		case 1:
			return 'ordinal';
		case 2:
			return 'quantitative';
		case 3:
			return 'temporal';
		default:
			return 'geojson';
	}
};
var $gicentre$elm_vegalite$VegaLite$operationSpec = function (op) {
	switch (op.$) {
		case 0:
			var maybeField = op.a;
			if (maybeField.$ === 1) {
				return $elm$json$Json$Encode$string('argmax');
			} else {
				var f = maybeField.a;
				return (!$elm$core$String$length(
					$elm$core$String$trim(f))) ? $elm$json$Json$Encode$string('argmax') : $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'argmax',
							$elm$json$Json$Encode$string(f))
						]));
			}
		case 1:
			var maybeField = op.a;
			if (maybeField.$ === 1) {
				return $elm$json$Json$Encode$string('argmin');
			} else {
				var f = maybeField.a;
				return (!$elm$core$String$length(
					$elm$core$String$trim(f))) ? $elm$json$Json$Encode$string('argmin') : $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'argmin',
							$elm$json$Json$Encode$string(f))
						]));
			}
		case 4:
			return $elm$json$Json$Encode$string('count');
		case 2:
			return $elm$json$Json$Encode$string('ci0');
		case 3:
			return $elm$json$Json$Encode$string('ci1');
		case 5:
			return $elm$json$Json$Encode$string('distinct');
		case 6:
			return $elm$json$Json$Encode$string('max');
		case 7:
			return $elm$json$Json$Encode$string('mean');
		case 8:
			return $elm$json$Json$Encode$string('median');
		case 9:
			return $elm$json$Json$Encode$string('min');
		case 10:
			return $elm$json$Json$Encode$string('missing');
		case 11:
			return $elm$json$Json$Encode$string('product');
		case 12:
			return $elm$json$Json$Encode$string('q1');
		case 13:
			return $elm$json$Json$Encode$string('q3');
		case 15:
			return $elm$json$Json$Encode$string('stdev');
		case 16:
			return $elm$json$Json$Encode$string('stdevp');
		case 17:
			return $elm$json$Json$Encode$string('sum');
		case 14:
			return $elm$json$Json$Encode$string('stderr');
		case 18:
			return $elm$json$Json$Encode$string('valid');
		case 19:
			return $elm$json$Json$Encode$string('variance');
		default:
			return $elm$json$Json$Encode$string('variancep');
	}
};
var $gicentre$elm_vegalite$VegaLite$cInterpolateSpec = function (iType) {
	switch (iType.$) {
		case 7:
			var gamma = iType.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('rgb')),
						_Utils_Tuple2(
						'gamma',
						$elm$json$Json$Encode$float(gamma))
					]));
		case 4:
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('hsl'))
					]));
		case 5:
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('hsl-long'))
					]));
		case 6:
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('lab'))
					]));
		case 2:
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('hcl'))
					]));
		case 3:
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('hcl-long'))
					]));
		case 0:
			var gamma = iType.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('cubehelix')),
						_Utils_Tuple2(
						'gamma',
						$elm$json$Json$Encode$float(gamma))
					]));
		default:
			var gamma = iType.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('cubehelix-long')),
						_Utils_Tuple2(
						'gamma',
						$elm$json$Json$Encode$float(gamma))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$channelLabel = function (ch) {
	switch (ch) {
		case 0:
			return 'x';
		case 1:
			return 'y';
		case 2:
			return 'x2';
		case 3:
			return 'y2';
		case 4:
			return 'xOffset';
		case 5:
			return 'yOffset';
		case 6:
			return 'color';
		case 7:
			return 'opacity';
		case 8:
			return 'shape';
		case 9:
			return 'size';
		default:
			return 'strokeDash';
	}
};
var $gicentre$elm_vegalite$VegaLite$scaleDomainSpec = function (sdType) {
	var numsSpec = function (ns) {
		if (!ns.$) {
			var xs = ns.a;
			return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs);
		} else {
			var s = ns.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
		}
	};
	var numSpec = function (n) {
		switch (n.$) {
			case 0:
				var x = n.a;
				return $elm$json$Json$Encode$float(x);
			case 1:
				return $elm$json$Json$Encode$null;
			default:
				var s = n.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'expr',
							$elm$json$Json$Encode$string(s))
						]));
		}
	};
	switch (sdType.$) {
		case 0:
			var xs = sdType.a;
			return numsSpec(xs);
		case 1:
			var x = sdType.a;
			return numSpec(x);
		case 2:
			var x = sdType.a;
			return numSpec(x);
		case 3:
			var x = sdType.a;
			return numSpec(x);
		case 7:
			var ds = sdType.a;
			return A2(
				$elm$json$Json$Encode$list,
				function (d) {
					return $elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d));
				},
				ds);
		case 8:
			var s = sdType.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
		case 4:
			var ts = sdType.a;
			if (!ts.$) {
				var d = ts.a;
				return $elm$json$Json$Encode$object(
					A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d));
			} else {
				var s = ts.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'expr',
							$elm$json$Json$Encode$string(s))
						]));
			}
		case 5:
			var ts = sdType.a;
			if (!ts.$) {
				var d = ts.a;
				return $elm$json$Json$Encode$object(
					A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d));
			} else {
				var s = ts.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'expr',
							$elm$json$Json$Encode$string(s))
						]));
			}
		case 6:
			var cats = sdType.a;
			if (!cats.$) {
				var ss = cats.a;
				return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, ss);
			} else {
				var s = cats.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'expr',
							$elm$json$Json$Encode$string(s))
						]));
			}
		case 9:
			var selName = sdType.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'param',
						$elm$json$Json$Encode$string(selName))
					]));
		case 11:
			var selName = sdType.a;
			var ch = sdType.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'param',
						$elm$json$Json$Encode$string(selName)),
						_Utils_Tuple2(
						'encoding',
						$elm$json$Json$Encode$string(
							$gicentre$elm_vegalite$VegaLite$channelLabel(ch)))
					]));
		case 10:
			var selName = sdType.a;
			var f = sdType.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'param',
						$elm$json$Json$Encode$string(selName)),
						_Utils_Tuple2(
						'field',
						$elm$json$Json$Encode$string(f))
					]));
		case 13:
			return $elm$json$Json$Encode$string('unaggregated');
		default:
			var scDo = sdType.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'unionWith',
						$gicentre$elm_vegalite$VegaLite$scaleDomainSpec(scDo))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$scaleLabel = function (sc) {
	switch (sc) {
		case 0:
			return 'linear';
		case 1:
			return 'pow';
		case 4:
			return 'symlog';
		case 2:
			return 'sqrt';
		case 3:
			return 'log';
		case 5:
			return 'time';
		case 6:
			return 'utc';
		case 7:
			return 'ordinal';
		case 8:
			return 'band';
		case 9:
			return 'point';
		case 10:
			return 'bin-ordinal';
		case 11:
			return 'quantile';
		case 12:
			return 'quantize';
		default:
			return 'threshold';
	}
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $gicentre$elm_vegalite$VegaLite$timeUnitLabel = function (tu) {
	switch (tu.$) {
		case 0:
			return 'year';
		case 13:
			return 'yeardayofyear';
		case 1:
			return 'yearquarter';
		case 2:
			return 'yearquartermonth';
		case 3:
			return 'yearmonth';
		case 4:
			return 'yearmonthdate';
		case 5:
			return 'yearmonthdatehours';
		case 6:
			return 'yearmonthdatehoursminutes';
		case 7:
			return 'yearmonthdatehoursminutesseconds';
		case 8:
			return 'yearweek';
		case 9:
			return 'yearweekday';
		case 10:
			return 'yearweekdayhours';
		case 11:
			return 'yearweekdayhoursminutes';
		case 12:
			return 'yearweekdayhoursminutesseconds';
		case 14:
			return 'quarter';
		case 15:
			return 'quartermonth';
		case 16:
			return 'month';
		case 17:
			return 'monthdate';
		case 18:
			return 'monthdatehours';
		case 19:
			return 'monthdatehoursminutes';
		case 20:
			return 'monthdatehoursminutesseconds';
		case 21:
			return 'week';
		case 22:
			return 'weekday';
		case 23:
			return 'weekdayhours';
		case 24:
			return 'weekdayhoursminutes';
		case 25:
			return 'weekdayhoursminutesseconds';
		case 26:
			return 'date';
		case 27:
			return 'day';
		case 28:
			return 'dayofyear';
		case 29:
			return 'dayhours';
		case 30:
			return 'dayhoursminutes';
		case 31:
			return 'dayhoursminutesseconds';
		case 32:
			return 'hours';
		case 33:
			return 'hoursminutes';
		case 34:
			return 'hoursminutesseconds';
		case 35:
			return 'minutes';
		case 36:
			return 'minutesseconds';
		case 37:
			return 'seconds';
		case 38:
			return 'secondsmilliseconds';
		case 39:
			return 'milliseconds';
		case 40:
			return '';
		case 41:
			return '';
		case 42:
			return '';
		default:
			return '';
	}
};
var $gicentre$elm_vegalite$VegaLite$scaleNiceSpec = function (ni) {
	switch (ni.$) {
		case 0:
			return $elm$json$Json$Encode$string('millisecond');
		case 1:
			return $elm$json$Json$Encode$string('second');
		case 2:
			return $elm$json$Json$Encode$string('minute');
		case 3:
			return $elm$json$Json$Encode$string('hour');
		case 4:
			return $elm$json$Json$Encode$string('day');
		case 5:
			return $elm$json$Json$Encode$string('week');
		case 6:
			return $elm$json$Json$Encode$string('month');
		case 7:
			return $elm$json$Json$Encode$string('year');
		case 10:
			var tu = ni.a;
			var step = ni.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'interval',
						$elm$json$Json$Encode$string(
							$gicentre$elm_vegalite$VegaLite$timeUnitLabel(tu))),
						_Utils_Tuple2(
						'step',
						$elm$json$Json$Encode$int(step))
					]));
		case 8:
			return $elm$json$Json$Encode$bool(true);
		case 9:
			return $elm$json$Json$Encode$bool(false);
		case 11:
			var n = ni.a;
			return $elm$json$Json$Encode$int(n);
		default:
			var s = ni.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$schemeProperty = F2(
	function (clrs, ext) {
		var nameSpec = function () {
			if (!clrs.$) {
				var ss = clrs.a;
				if (!ss.b) {
					return A2(
						$elm$json$Json$Encode$list,
						$elm$json$Json$Encode$string,
						_List_fromArray(
							['rgb(86,119,164)', 'rgb(86,119,164)']));
				} else {
					if (!ss.b.b) {
						var sch = ss.a;
						return $elm$json$Json$Encode$string(sch);
					} else {
						return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, ss);
					}
				}
			} else {
				var ex = clrs.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'expr',
							$elm$json$Json$Encode$string(ex))
						]));
			}
		}();
		if (!ext.b) {
			return _Utils_Tuple2('scheme', nameSpec);
		} else {
			if (!ext.b.b) {
				var n = ext.a;
				return _Utils_Tuple2(
					'scheme',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2('name', nameSpec),
								_Utils_Tuple2(
								'count',
								$elm$json$Json$Encode$float(n))
							])));
			} else {
				if (!ext.b.b.b) {
					var mn = ext.a;
					var _v1 = ext.b;
					var mx = _v1.a;
					return _Utils_Tuple2(
						'scheme',
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2('name', nameSpec),
									_Utils_Tuple2(
									'extent',
									A2(
										$elm$json$Json$Encode$list,
										$elm$json$Json$Encode$float,
										_List_fromArray(
											[mn, mx])))
								])));
				} else {
					return _Utils_Tuple2('scheme', nameSpec);
				}
			}
		}
	});
var $gicentre$elm_vegalite$VegaLite$strsExpr = F2(
	function (objName, ss) {
		if (!ss.$) {
			var xs = ss.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					objName,
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, xs))
				]);
		} else {
			var s = ss.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					objName,
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(s))
							])))
				]);
		}
	});
var $gicentre$elm_vegalite$VegaLite$scaleProperty = function (scaleProp) {
	switch (scaleProp.$) {
		case 0:
			var sType = scaleProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$scaleLabel(sType)))
				]);
		case 2:
			var s = scaleProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'domainRaw',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(s))
							])))
				]);
		case 1:
			var sdType = scaleProp.a;
			switch (sdType.$) {
				case 1:
					var x = sdType.a;
					return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'domainMin', x);
				case 2:
					var x = sdType.a;
					return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'domainMid', x);
				case 3:
					var x = sdType.a;
					return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'domainMax', x);
				case 4:
					var ts = sdType.a;
					if (!ts.$) {
						var d = ts.a;
						return _List_fromArray(
							[
								_Utils_Tuple2(
								'domainMin',
								$elm$json$Json$Encode$object(
									A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d)))
							]);
					} else {
						var s = ts.a;
						return _List_fromArray(
							[
								_Utils_Tuple2(
								'domainMin',
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'expr',
											$elm$json$Json$Encode$string(s))
										])))
							]);
					}
				case 5:
					var ts = sdType.a;
					if (!ts.$) {
						var d = ts.a;
						return _List_fromArray(
							[
								_Utils_Tuple2(
								'domainMax',
								$elm$json$Json$Encode$object(
									A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d)))
							]);
					} else {
						var s = ts.a;
						return _List_fromArray(
							[
								_Utils_Tuple2(
								'domainMax',
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'expr',
											$elm$json$Json$Encode$string(s))
										])))
							]);
					}
				default:
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'domain',
							$gicentre$elm_vegalite$VegaLite$scaleDomainSpec(sdType))
						]);
			}
		case 3:
			var range = scaleProp.a;
			switch (range.$) {
				case 5:
					var x = range.a;
					return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'rangeMin', x);
				case 6:
					var x = range.a;
					return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'rangeMax', x);
				case 0:
					var xs = range.a;
					return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'range', xs);
				case 2:
					var ss = range.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'range',
							A2(
								$elm$json$Json$Encode$list,
								function (s) {
									return $elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'expr',
												$elm$json$Json$Encode$string(s))
											]));
								},
								ss))
						]);
				case 3:
					var xss = range.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'range',
							A2(
								$elm$json$Json$Encode$list,
								$elm$json$Json$Encode$list($elm$json$Json$Encode$float),
								xss))
						]);
				case 1:
					var ss = range.a;
					return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'range', ss);
				case 4:
					var s = range.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'range',
							$elm$json$Json$Encode$string(s))
						]);
				default:
					var s = range.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'range',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'field',
										$elm$json$Json$Encode$string(s))
									])))
						]);
			}
		case 4:
			var schName = scaleProp.a;
			var ext = scaleProp.b;
			return _List_fromArray(
				[
					A2($gicentre$elm_vegalite$VegaLite$schemeProperty, schName, ext)
				]);
		case 5:
			var schExpr = scaleProp.a;
			var ext = scaleProp.b;
			return _List_fromArray(
				[
					A2($gicentre$elm_vegalite$VegaLite$schemeProperty, schExpr, ext)
				]);
		case 6:
			var x = scaleProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'align', x);
		case 7:
			var x = scaleProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'padding', x);
		case 17:
			var x = scaleProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'base', x);
		case 15:
			var x = scaleProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'exponent', x);
		case 16:
			var x = scaleProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'constant', x);
		case 8:
			var x = scaleProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'paddingInner', x);
		case 9:
			var x = scaleProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'paddingOuter', x);
		case 10:
			var b = scaleProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'round', b);
		case 11:
			var b = scaleProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'clamp', b);
		case 12:
			var interp = scaleProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'interpolate',
					$gicentre$elm_vegalite$VegaLite$cInterpolateSpec(interp))
				]);
		case 13:
			var ni = scaleProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'nice',
					$gicentre$elm_vegalite$VegaLite$scaleNiceSpec(ni))
				]);
		case 14:
			var b = scaleProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'zero', b);
		default:
			var b = scaleProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'reverse', b);
	}
};
var $gicentre$elm_vegalite$VegaLite$sortProperties = function (sp) {
	switch (sp.$) {
		case 0:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'order',
					$elm$json$Json$Encode$string('ascending'))
				]);
		case 1:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'order',
					$elm$json$Json$Encode$string('descending'))
				]);
		case 5:
			var ch = sp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'encoding',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$channelLabel(ch)))
				]);
		case 4:
			var field = sp.a;
			var op = sp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(field)),
					_Utils_Tuple2(
					'op',
					$gicentre$elm_vegalite$VegaLite$operationSpec(op))
				]);
		case 3:
			var arr = sp.a;
			var op = sp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'repeat',
								$elm$json$Json$Encode$string(
									$gicentre$elm_vegalite$VegaLite$arrangementLabel(arr)))
							]))),
					_Utils_Tuple2(
					'op',
					$gicentre$elm_vegalite$VegaLite$operationSpec(op))
				]);
		default:
			return _List_Nil;
	}
};
var $gicentre$elm_vegalite$VegaLite$timeUnitProperties = function (tUnit) {
	switch (tUnit.$) {
		case 40:
			var tu = tUnit.a;
			return A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'utc',
					$elm$json$Json$Encode$bool(true)),
				$gicentre$elm_vegalite$VegaLite$timeUnitProperties(tu));
		case 42:
			var n = tUnit.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'maxbins',
					$elm$json$Json$Encode$int(n))
				]);
		case 43:
			var x = tUnit.a;
			var tu = tUnit.b;
			return A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'step',
					$elm$json$Json$Encode$float(x)),
				$gicentre$elm_vegalite$VegaLite$timeUnitProperties(tu));
		default:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'unit',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$timeUnitLabel(tUnit)))
				]);
	}
};
var $gicentre$elm_vegalite$VegaLite$timeUnitSpec = function (tUnit) {
	if (tUnit.$ === 41) {
		var tu = tUnit.a;
		return $elm$json$Json$Encode$string(
			'binned' + $gicentre$elm_vegalite$VegaLite$timeUnitLabel(tu));
	} else {
		return $elm$json$Json$Encode$object(
			$gicentre$elm_vegalite$VegaLite$timeUnitProperties(tUnit));
	}
};
var $gicentre$elm_vegalite$VegaLite$booleanOpSpec = function (bo) {
	switch (bo.$) {
		case 0:
			var ex = bo.a;
			return $elm$json$Json$Encode$string(ex);
		case 1:
			var f = bo.a;
			return $gicentre$elm_vegalite$VegaLite$filterSpec(f);
		case 2:
			var tr = bo.a;
			var f = bo.b;
			return A2($gicentre$elm_vegalite$VegaLite$trFilterSpec, tr, f);
		case 4:
			var operand1 = bo.a;
			var operand2 = bo.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'and',
						A2(
							$elm$json$Json$Encode$list,
							$gicentre$elm_vegalite$VegaLite$booleanOpSpec,
							_List_fromArray(
								[operand1, operand2])))
					]));
		case 5:
			var operand1 = bo.a;
			var operand2 = bo.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'or',
						A2(
							$elm$json$Json$Encode$list,
							$gicentre$elm_vegalite$VegaLite$booleanOpSpec,
							_List_fromArray(
								[operand1, operand2])))
					]));
		case 6:
			var operand = bo.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'not',
						$gicentre$elm_vegalite$VegaLite$booleanOpSpec(operand))
					]));
		case 3:
			var p = bo.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'param',
						$elm$json$Json$Encode$string(p))
					]));
		case 8:
			var selName = bo.a;
			return $elm$json$Json$Encode$string(selName);
		default:
			var sel = bo.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'selection',
						$elm$json$Json$Encode$string(sel))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$filterSpec = function (f) {
	switch (f.$) {
		case 5:
			var ex = f.a;
			return $elm$json$Json$Encode$string(ex);
		case 6:
			var boolExpr = f.a;
			return $gicentre$elm_vegalite$VegaLite$booleanOpSpec(boolExpr);
		default:
			return $elm$json$Json$Encode$object(
				$gicentre$elm_vegalite$VegaLite$filterProperties(f));
	}
};
var $gicentre$elm_vegalite$VegaLite$markChannelProperties = function (field) {
	switch (field.$) {
		case 0:
			var s = field.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'field', s);
		case 3:
			var d = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'datum',
					$gicentre$elm_vegalite$VegaLite$dataValueSpec(d))
				]);
		case 4:
			var arr = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'repeat',
								$elm$json$Json$Encode$string(
									$gicentre$elm_vegalite$VegaLite$arrangementLabel(arr)))
							])))
				]);
		case 5:
			var arr = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'datum',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'repeat',
								$elm$json$Json$Encode$string(
									$gicentre$elm_vegalite$VegaLite$arrangementLabel(arr)))
							])))
				]);
		case 6:
			var t = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$measurementLabel(t)))
				]);
		case 7:
			var sps = field.a;
			return _Utils_eq(sps, _List_Nil) ? _List_fromArray(
				[
					_Utils_Tuple2('scale', $elm$json$Json$Encode$null)
				]) : _List_fromArray(
				[
					_Utils_Tuple2(
					'scale',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$scaleProperty, sps)))
				]);
		case 15:
			var lps = field.a;
			return _Utils_eq(lps, _List_Nil) ? _List_fromArray(
				[
					_Utils_Tuple2('legend', $elm$json$Json$Encode$null)
				]) : _List_fromArray(
				[
					_Utils_Tuple2(
					'legend',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$legendProperty, lps)))
				]);
		case 9:
			var bps = field.a;
			return _List_fromArray(
				[
					$gicentre$elm_vegalite$VegaLite$bin(bps)
				]);
		case 8:
			var x = field.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'band', x);
		case 11:
			var sps = field.a;
			_v3$4:
			while (true) {
				if (!sps.b) {
					return _List_fromArray(
						[
							_Utils_Tuple2('sort', $elm$json$Json$Encode$null)
						]);
				} else {
					if (!sps.b.b) {
						switch (sps.a.$) {
							case 0:
								var _v4 = sps.a;
								return _List_fromArray(
									[
										_Utils_Tuple2(
										'sort',
										$elm$json$Json$Encode$string('ascending'))
									]);
							case 1:
								var _v5 = sps.a;
								return _List_fromArray(
									[
										_Utils_Tuple2(
										'sort',
										$elm$json$Json$Encode$string('descending'))
									]);
							case 2:
								var dvs = sps.a.a;
								return _List_fromArray(
									[
										_Utils_Tuple2(
										'sort',
										$gicentre$elm_vegalite$VegaLite$dataValuesSpecs(dvs))
									]);
							default:
								break _v3$4;
						}
					} else {
						break _v3$4;
					}
				}
			}
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'sort',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$sortProperties, sps)))
				]);
		case 10:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'bin',
					$elm$json$Json$Encode$string('binned'))
				]);
		case 1:
			var predicate = field.a;
			var ifClause = field.b;
			var elseClause = field.c;
			return A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'condition',
					$elm$json$Json$Encode$object(
						_Utils_ap(
							$gicentre$elm_vegalite$VegaLite$predicateProperties(predicate),
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markChannelProperties, ifClause)))),
				A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markChannelProperties, elseClause));
		case 2:
			var ifClauses = field.a;
			var elseClause = field.b;
			return A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'condition',
					A2(
						$elm$json$Json$Encode$list,
						function (_v6) {
							var predicate = _v6.a;
							var ifClause = _v6.b;
							return $elm$json$Json$Encode$object(
								_Utils_ap(
									$gicentre$elm_vegalite$VegaLite$predicateProperties(predicate),
									A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markChannelProperties, ifClause)));
						},
						ifClauses)),
				A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markChannelProperties, elseClause));
		case 12:
			var tu = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'timeUnit',
					$gicentre$elm_vegalite$VegaLite$timeUnitSpec(tu))
				]);
		case 13:
			var s = field.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExprMultiline, 'title', s);
		case 14:
			var op = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'aggregate',
					$gicentre$elm_vegalite$VegaLite$operationSpec(op))
				]);
		case 16:
			var s = field.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'value', s);
		case 17:
			var x = field.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'value', x);
		case 18:
			var s = field.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'value', s);
		default:
			var b = field.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'value', b);
	}
};
var $gicentre$elm_vegalite$VegaLite$predicateProperties = function (predicate) {
	switch (predicate.$) {
		case 0:
			var p = predicate.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'param',
					$elm$json$Json$Encode$string(p))
				]);
		case 1:
			var p = predicate.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'param',
					$elm$json$Json$Encode$string(p)),
					_Utils_Tuple2(
					'empty',
					$elm$json$Json$Encode$bool(false))
				]);
		default:
			var bo = predicate.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'test',
					$gicentre$elm_vegalite$VegaLite$booleanOpSpec(bo))
				]);
	}
};
var $gicentre$elm_vegalite$VegaLite$trFilterSpec = F2(
	function (mc, f) {
		switch (f.$) {
			case 5:
				var ex = f.a;
				return $elm$json$Json$Encode$string(ex);
			case 6:
				var boolExpr = f.a;
				return $gicentre$elm_vegalite$VegaLite$booleanOpSpec(boolExpr);
			default:
				return $elm$json$Json$Encode$object(
					_Utils_ap(
						$gicentre$elm_vegalite$VegaLite$markChannelProperties(mc),
						$gicentre$elm_vegalite$VegaLite$filterProperties(f)));
		}
	});
var $gicentre$elm_vegalite$VegaLite$color = function (markProps) {
	return $elm$core$List$cons(
		_Utils_Tuple2(
			'color',
			$elm$json$Json$Encode$object(
				A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markChannelProperties, markProps))));
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $gicentre$elm_vegalite$VegaLite$autosizeProperty = function (asCfg) {
	switch (asCfg) {
		case 5:
			return _Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string('pad'));
		case 1:
			return _Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string('fit'));
		case 2:
			return _Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string('fit-x'));
		case 3:
			return _Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string('fit-y'));
		case 4:
			return _Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string('none'));
		case 7:
			return _Utils_Tuple2(
				'resize',
				$elm$json$Json$Encode$bool(true));
		case 0:
			return _Utils_Tuple2(
				'contains',
				$elm$json$Json$Encode$string('content'));
		default:
			return _Utils_Tuple2(
				'contains',
				$elm$json$Json$Encode$string('padding'));
	}
};
var $gicentre$elm_vegalite$VegaLite$maybeNumExpr = F2(
	function (objName, n) {
		if (!n.$) {
			var maybeX = n.a;
			if (!maybeX.$) {
				var x = maybeX.a;
				return (x === 1) ? _List_fromArray(
					[
						_Utils_Tuple2(
						objName,
						$elm$json$Json$Encode$bool(true))
					]) : _List_fromArray(
					[
						_Utils_Tuple2(
						objName,
						$elm$json$Json$Encode$float(x))
					]);
			} else {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						objName,
						$elm$json$Json$Encode$bool(false))
					]);
			}
		} else {
			var s = n.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					objName,
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(s))
							])))
				]);
		}
	});
var $gicentre$elm_vegalite$VegaLite$tickBandSpec = function (tb) {
	switch (tb.$) {
		case 0:
			return $elm$json$Json$Encode$string('center');
		case 1:
			return $elm$json$Json$Encode$string('extent');
		default:
			var s = tb.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$axisConfigProperty = function (axisCfg) {
	switch (axisCfg.$) {
		case 40:
			var ss = axisCfg.a;
			if (!ss.$) {
				var xs = ss.a;
				if (xs.b && (!xs.b.b)) {
					var s = xs.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'style',
							$elm$json$Json$Encode$string(s))
						]);
				} else {
					return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'style', ss);
				}
			} else {
				return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'style', ss);
			}
		case 0:
			var aps = axisCfg.a;
			if (!aps.b) {
				return $gicentre$elm_vegalite$VegaLite$ariaProperty(
					$gicentre$elm_vegalite$VegaLite$ArAria(
						$gicentre$elm_vegalite$VegaLite$Boo(false)));
			} else {
				return A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$ariaProperty, aps);
			}
		case 2:
			var b = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'disable', b);
		case 1:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'bandPosition', x);
		case 3:
			var b = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'domain', b);
		case 4:
			var c = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'domainCap',
					$gicentre$elm_vegalite$VegaLite$strokeCapSpec(c))
				]);
		case 5:
			var s = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'domainColor', s);
		case 6:
			var xs = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'domainDash', xs);
		case 7:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'domainDashOffset', x);
		case 8:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'domainOpacity', x);
		case 9:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'domainWidth', x);
		case 37:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'maxExtent', x);
		case 38:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'minExtent', x);
		case 39:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'offset', x);
		case 10:
			var b = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'grid', b);
		case 11:
			var c = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'gridCap',
					$gicentre$elm_vegalite$VegaLite$strokeCapSpec(c))
				]);
		case 12:
			var s = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'gridColor', s);
		case 13:
			var xs = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'gridDash', xs);
		case 14:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gridDashOffset', x);
		case 15:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gridOpacity', x);
		case 16:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gridWidth', x);
		case 17:
			var b = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'labels', b);
		case 18:
			var ha = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelAlign',
					$gicentre$elm_vegalite$VegaLite$hAlignSpec(ha))
				]);
		case 19:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelAngle', x);
		case 20:
			var va = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelBaseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 21:
			var mn = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$maybeNumExpr, 'labelBound', mn);
		case 24:
			var s = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelColor', s);
		case 25:
			var s = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelExpr', s);
		case 22:
			var n = axisCfg.a;
			switch (n.$) {
				case 0:
					var x = n.a;
					return (!x) ? _List_fromArray(
						[
							_Utils_Tuple2(
							'labelFlush',
							$elm$json$Json$Encode$bool(true))
						]) : A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelFlush', n);
				case 1:
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'labelFlush',
							$elm$json$Json$Encode$bool(false))
						]);
				default:
					return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelFlush', n);
			}
		case 23:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelFlushOffset', x);
		case 26:
			var s = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelFont', s);
		case 28:
			var s = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelFontStyle', s);
		case 27:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelFontSize', x);
		case 29:
			var fw = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelFontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw))
				]);
		case 30:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelLimit', x);
		case 31:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelLineHeight', x);
		case 32:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelOffset', x);
		case 33:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelOpacity', x);
		case 34:
			var strat = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelOverlap',
					$gicentre$elm_vegalite$VegaLite$overlapStrategySpec(strat))
				]);
		case 35:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelPadding', x);
		case 36:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelSeparation', x);
		case 41:
			var b = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'ticks', b);
		case 42:
			var tb = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'tickBand',
					$gicentre$elm_vegalite$VegaLite$tickBandSpec(tb))
				]);
		case 43:
			var c = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'tickCap',
					$gicentre$elm_vegalite$VegaLite$strokeCapSpec(c))
				]);
		case 44:
			var s = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'tickColor', s);
		case 45:
			var tc = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'tickCount',
					$gicentre$elm_vegalite$VegaLite$scaleNiceSpec(tc))
				]);
		case 46:
			var xs = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'tickDash', xs);
		case 47:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickDashOffset', x);
		case 48:
			var b = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'tickExtra', b);
		case 49:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickOffset', x);
		case 50:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickOpacity', x);
		case 53:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickMinStep', x);
		case 51:
			var b = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'tickRound', b);
		case 52:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickSize', x);
		case 54:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickWidth', x);
		case 55:
			var al = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleAlign',
					$gicentre$elm_vegalite$VegaLite$hAlignSpec(al))
				]);
		case 57:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleAngle', x);
		case 56:
			var an = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleAnchor',
					$gicentre$elm_vegalite$VegaLite$anchorSpec(an))
				]);
		case 58:
			var va = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleBaseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 59:
			var s = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleColor', s);
		case 60:
			var s = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleFont', s);
		case 62:
			var s = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleFontStyle', s);
		case 63:
			var w = axisCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleFontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(w))
				]);
		case 61:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleFontSize', x);
		case 64:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleLimit', x);
		case 65:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleLineHeight', x);
		case 66:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleOpacity', x);
		case 67:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titlePadding', x);
		case 68:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleX', x);
		case 69:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleY', x);
		default:
			var x = axisCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'translate', x);
	}
};
var $gicentre$elm_vegalite$VegaLite$axisLabel = function (axChoice) {
	switch (axChoice) {
		case 1:
			return 'axisX';
		case 2:
			return 'axisY';
		default:
			return 'axis';
	}
};
var $gicentre$elm_vegalite$VegaLite$AxGridColor = function (a) {
	return {$: 74, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxGridDash = function (a) {
	return {$: 75, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxGridDashOffset = function (a) {
	return {$: 76, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxGridOpacity = function (a) {
	return {$: 77, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxGridWidth = function (a) {
	return {$: 78, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxLabelAlign = function (a) {
	return {$: 21, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxLabelColor = function (a) {
	return {$: 25, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxLabelFont = function (a) {
	return {$: 29, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxLabelFontSize = function (a) {
	return {$: 30, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxLabelFontStyle = function (a) {
	return {$: 31, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxLabelOffset = function (a) {
	return {$: 35, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxLabelOpacity = function (a) {
	return {$: 36, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxLabelPadding = function (a) {
	return {$: 38, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxTickColor = function (a) {
	return {$: 44, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxTickDash = function (a) {
	return {$: 46, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxTickDashOffset = function (a) {
	return {$: 47, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxTickOpacity = function (a) {
	return {$: 50, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxTickSize = function (a) {
	return {$: 53, a: a};
};
var $gicentre$elm_vegalite$VegaLite$AxTickWidth = function (a) {
	return {$: 54, a: a};
};
var $gicentre$elm_vegalite$VegaLite$axLabelAlign = $gicentre$elm_vegalite$VegaLite$AxLabelAlign;
var $gicentre$elm_vegalite$VegaLite$AxLabelBaseline = function (a) {
	return {$: 23, a: a};
};
var $gicentre$elm_vegalite$VegaLite$axLabelBaseline = $gicentre$elm_vegalite$VegaLite$AxLabelBaseline;
var $gicentre$elm_vegalite$VegaLite$AxLabelFontWeight = function (a) {
	return {$: 32, a: a};
};
var $gicentre$elm_vegalite$VegaLite$axLabelFontWeight = $gicentre$elm_vegalite$VegaLite$AxLabelFontWeight;
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $gicentre$elm_vegalite$VegaLite$sideSpec = function (side) {
	switch (side.$) {
		case 0:
			return $elm$json$Json$Encode$string('top');
		case 1:
			return $elm$json$Json$Encode$string('bottom');
		case 2:
			return $elm$json$Json$Encode$string('left');
		case 3:
			return $elm$json$Json$Encode$string('right');
		default:
			var ex = side.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(ex))
					]));
	}
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $gicentre$elm_vegalite$VegaLite$axisProperty = function (axisProp) {
	switch (axisProp.$) {
		case 0:
			var aps = axisProp.a;
			if (!aps.b) {
				return $gicentre$elm_vegalite$VegaLite$ariaProperty(
					$gicentre$elm_vegalite$VegaLite$ArAria(
						$gicentre$elm_vegalite$VegaLite$Boo(false)));
			} else {
				return A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$ariaProperty, aps);
			}
		case 1:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'bandPosition', n);
		case 80:
			var predicate = axisProp.a;
			var cap = axisProp.b;
			var firstProp = A2(
				$elm$core$Basics$composeR,
				$elm$core$List$head,
				$elm$core$Maybe$withDefault(
					_Utils_Tuple2('', $elm$json$Json$Encode$null)));
			var _v2 = function () {
				switch (cap.$) {
					case 0:
						var ha1 = cap.a;
						var ha2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelAlign(ha1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$axLabelAlign(ha2))));
					case 1:
						var va1 = cap.a;
						var va2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$axLabelBaseline(va1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$axLabelBaseline(va2))));
					case 2:
						var c1 = cap.a;
						var c2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelColor(c1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelColor(c2))));
					case 3:
						var f1 = cap.a;
						var f2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelFont(f1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelFont(f2))));
					case 4:
						var s1 = cap.a;
						var s2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelFontSize(s1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelFontSize(s2))));
					case 5:
						var s1 = cap.a;
						var s2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelFontStyle(s1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelFontStyle(s2))));
					case 6:
						var w1 = cap.a;
						var w2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$axLabelFontWeight(w1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$axLabelFontWeight(w2))));
					case 7:
						var o1 = cap.a;
						var o2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelOffset(o1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelOffset(o2))));
					case 8:
						var o1 = cap.a;
						var o2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelOpacity(o1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelOpacity(o2))));
					case 9:
						var p1 = cap.a;
						var p2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelPadding(p1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxLabelPadding(p2))));
					case 10:
						var c1 = cap.a;
						var c2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickColor(c1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickColor(c2))));
					case 11:
						var d1 = cap.a;
						var d2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickDash(d1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickDash(d2))));
					case 12:
						var o1 = cap.a;
						var o2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickDashOffset(o1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickDashOffset(o2))));
					case 13:
						var o1 = cap.a;
						var o2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickOpacity(o1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickOpacity(o2))));
					case 19:
						var s1 = cap.a;
						var s2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickSize(s1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickSize(s2))));
					case 14:
						var w1 = cap.a;
						var w2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickWidth(w1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxTickWidth(w2))));
					case 15:
						var c1 = cap.a;
						var c2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxGridColor(c1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxGridColor(c2))));
					case 16:
						var d1 = cap.a;
						var d2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxGridDash(d1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxGridDash(d2))));
					case 17:
						var o1 = cap.a;
						var o2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxGridDashOffset(o1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxGridDashOffset(o2))));
					case 18:
						var o1 = cap.a;
						var o2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxGridOpacity(o1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxGridOpacity(o2))));
					default:
						var w1 = cap.a;
						var w2 = cap.b;
						return _Utils_Tuple2(
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxGridWidth(w1))),
							firstProp(
								$gicentre$elm_vegalite$VegaLite$axisProperty(
									$gicentre$elm_vegalite$VegaLite$AxGridWidth(w2))));
				}
			}();
			var ifProp = _v2.a;
			var elseProp = _v2.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					ifProp.a,
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'condition',
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'test',
											$gicentre$elm_vegalite$VegaLite$booleanOpSpec(predicate)),
											_Utils_Tuple2('value', ifProp.b)
										]))),
								_Utils_Tuple2('value', elseProp.b)
							])))
				]);
		case 15:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'format', s);
		case 16:
			var fmts = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'format',
					$elm$json$Json$Encode$object(
						A2(
							$elm$core$List$map,
							function (_v4) {
								var tu = _v4.a;
								var s = _v4.b;
								return _Utils_Tuple2(
									$gicentre$elm_vegalite$VegaLite$timeUnitLabel(tu),
									$elm$json$Json$Encode$string(s));
							},
							fmts)))
				]);
		case 17:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'formatType',
					$elm$json$Json$Encode$string('number'))
				]);
		case 18:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'formatType',
					$elm$json$Json$Encode$string('time'))
				]);
		case 19:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'formatType', s);
		case 73:
			var c = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'gridCap',
					$gicentre$elm_vegalite$VegaLite$strokeCapSpec(c))
				]);
		case 74:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'gridColor', s);
		case 75:
			var ns = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'gridDash', ns);
		case 76:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gridDashOffset', n);
		case 77:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gridOpacity', n);
		case 78:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gridWidth', n);
		case 20:
			var b = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'labels', b);
		case 21:
			var ha = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelAlign',
					$gicentre$elm_vegalite$VegaLite$hAlignSpec(ha))
				]);
		case 23:
			var va = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelBaseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 24:
			var mn = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$maybeNumExpr, 'labelBound', mn);
		case 22:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelAngle', n);
		case 25:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelColor', s);
		case 26:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelExpr', s);
		case 27:
			var n = axisProp.a;
			switch (n.$) {
				case 0:
					var x = n.a;
					return (!x) ? _List_fromArray(
						[
							_Utils_Tuple2(
							'labelFlush',
							$elm$json$Json$Encode$bool(true))
						]) : A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelFlush', n);
				case 1:
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'labelFlush',
							$elm$json$Json$Encode$bool(false))
						]);
				default:
					return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelFlush', n);
			}
		case 28:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelFlushOffset', n);
		case 29:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelFont', s);
		case 30:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelFontSize', n);
		case 31:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelFontStyle', s);
		case 32:
			var fw = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelFontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw))
				]);
		case 34:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelLimit', n);
		case 33:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelLineHeight', n);
		case 35:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelOffset', n);
		case 36:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelOpacity', n);
		case 37:
			var strat = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelOverlap',
					$gicentre$elm_vegalite$VegaLite$overlapStrategySpec(strat))
				]);
		case 38:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelPadding', n);
		case 39:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelSeparation', n);
		case 8:
			var b = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'domain', b);
		case 9:
			var c = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'domainCap',
					$gicentre$elm_vegalite$VegaLite$strokeCapSpec(c))
				]);
		case 10:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'domainColor', s);
		case 11:
			var ns = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'domainDash', ns);
		case 12:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'domainDashOffset', n);
		case 13:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'domainOpacity', n);
		case 14:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'domainWidth', n);
		case 72:
			var b = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'grid', b);
		case 2:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'maxExtent', n);
		case 3:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'minExtent', n);
		case 4:
			var side = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'orient',
					$gicentre$elm_vegalite$VegaLite$sideSpec(side))
				]);
		case 5:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'offset', n);
		case 6:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'position', n);
		case 41:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'translate', n);
		case 40:
			var ss = axisProp.a;
			if (!ss.$) {
				var xs = ss.a;
				if (xs.b && (!xs.b.b)) {
					var s = xs.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'style',
							$elm$json$Json$Encode$string(s))
						]);
				} else {
					return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'style', ss);
				}
			} else {
				return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'style', ss);
			}
		case 7:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'zindex', n);
		case 52:
			var b = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'ticks', b);
		case 42:
			var tb = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'tickBand',
					$gicentre$elm_vegalite$VegaLite$tickBandSpec(tb))
				]);
		case 43:
			var c = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'tickCap',
					$gicentre$elm_vegalite$VegaLite$strokeCapSpec(c))
				]);
		case 44:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'tickColor', s);
		case 45:
			var tc = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'tickCount',
					$gicentre$elm_vegalite$VegaLite$scaleNiceSpec(tc))
				]);
		case 46:
			var ns = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'tickDash', ns);
		case 47:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickDashOffset', n);
		case 48:
			var b = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'tickExtra', b);
		case 49:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickOffset', n);
		case 50:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickOpacity', n);
		case 51:
			var b = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'tickRound', b);
		case 79:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickMinStep', n);
		case 53:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickSize', n);
		case 54:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tickWidth', n);
		case 55:
			var vals = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'values',
					$gicentre$elm_vegalite$VegaLite$dataValuesSpecs(vals))
				]);
		case 56:
			var s = axisProp.a;
			if (!s.$) {
				var ttl = s.a;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'title',
						$gicentre$elm_vegalite$VegaLite$multilineTextSpec(ttl))
					]);
			} else {
				return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'title', s);
			}
		case 57:
			var al = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleAlign',
					$gicentre$elm_vegalite$VegaLite$hAlignSpec(al))
				]);
		case 59:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleAngle', n);
		case 58:
			var an = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleAnchor',
					$gicentre$elm_vegalite$VegaLite$anchorSpec(an))
				]);
		case 60:
			var va = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleBaseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 61:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleColor', s);
		case 62:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleFont', s);
		case 63:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleFontSize', n);
		case 64:
			var s = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleFontStyle', s);
		case 65:
			var fw = axisProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleFontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw))
				]);
		case 66:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleLimit', n);
		case 67:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleLineHeight', n);
		case 68:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleOpacity', n);
		case 69:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titlePadding', n);
		case 70:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleX', n);
		default:
			var n = axisProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleY', n);
	}
};
var $gicentre$elm_vegalite$VegaLite$concatConfigProperty = function (ccp) {
	if (!ccp.$) {
		var x = ccp.a;
		return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'columns', x);
	} else {
		var x = ccp.a;
		return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'spacing', x);
	}
};
var $gicentre$elm_vegalite$VegaLite$facetConfigProperty = function (fcp) {
	if (!fcp.$) {
		var x = fcp.a;
		return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'columns', x);
	} else {
		var x = fcp.a;
		return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'spacing', x);
	}
};
var $gicentre$elm_vegalite$VegaLite$fieldTitleLabel = function (ftp) {
	switch (ftp) {
		case 0:
			return 'verbal';
		case 1:
			return 'functional';
		default:
			return 'plain';
	}
};
var $gicentre$elm_vegalite$VegaLite$NoStr = {$: 1};
var $gicentre$elm_vegalite$VegaLite$headerProperty = function (hProp) {
	switch (hProp.$) {
		case 0:
			var s = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'format', s);
		case 1:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'formatType',
					$elm$json$Json$Encode$string('number'))
				]);
		case 2:
			return A2(
				$gicentre$elm_vegalite$VegaLite$strExpr,
				'formatType',
				$gicentre$elm_vegalite$VegaLite$Str('time'));
		case 3:
			var s = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'formatType', s);
		case 5:
			var ha = hProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelAlign',
					$gicentre$elm_vegalite$VegaLite$hAlignSpec(ha))
				]);
		case 6:
			var a = hProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelAnchor',
					$gicentre$elm_vegalite$VegaLite$anchorSpec(a))
				]);
		case 7:
			var x = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelAngle', x);
		case 8:
			var va = hProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelBaseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 9:
			var s = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelColor', s);
		case 10:
			var s = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelExpr', s);
		case 11:
			var s = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelFont', s);
		case 12:
			var x = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelFontSize', x);
		case 13:
			var s = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelFontStyle', s);
		case 14:
			var fw = hProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelFontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw))
				]);
		case 15:
			var x = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelLimit', x);
		case 16:
			var x = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelLineHeight', x);
		case 17:
			var orient = hProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelOrient',
					$gicentre$elm_vegalite$VegaLite$sideSpec(orient))
				]);
		case 18:
			var x = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelPadding', x);
		case 19:
			var b = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'labels', b);
		case 20:
			var orient = hProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'orient',
					$gicentre$elm_vegalite$VegaLite$sideSpec(orient))
				]);
		case 4:
			var ttl = hProp.a;
			if (!ttl.$) {
				var s = ttl.a;
				if (s === '') {
					return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'title', $gicentre$elm_vegalite$VegaLite$NoStr);
				} else {
					return A2($gicentre$elm_vegalite$VegaLite$strExprMultiline, 'title', ttl);
				}
			} else {
				return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'title', ttl);
			}
		case 22:
			var a = hProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleAnchor',
					$gicentre$elm_vegalite$VegaLite$anchorSpec(a))
				]);
		case 21:
			var ha = hProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleAlign',
					$gicentre$elm_vegalite$VegaLite$hAlignSpec(ha))
				]);
		case 23:
			var x = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleAngle', x);
		case 24:
			var va = hProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleBaseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 25:
			var s = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleColor', s);
		case 26:
			var s = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleFont', s);
		case 29:
			var fw = hProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleFontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw))
				]);
		case 27:
			var x = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleFontSize', x);
		case 28:
			var s = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleFontStyle', s);
		case 30:
			var x = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleLimit', x);
		case 31:
			var x = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleLineHeight', x);
		case 32:
			var orient = hProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleOrient',
					$gicentre$elm_vegalite$VegaLite$sideSpec(orient))
				]);
		default:
			var x = hProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titlePadding', x);
	}
};
var $gicentre$elm_vegalite$VegaLite$legendConfigProperty = function (legendConfig) {
	switch (legendConfig.$) {
		case 0:
			var aps = legendConfig.a;
			if (!aps.b) {
				return $gicentre$elm_vegalite$VegaLite$ariaProperty(
					$gicentre$elm_vegalite$VegaLite$ArAria(
						$gicentre$elm_vegalite$VegaLite$Boo(false)));
			} else {
				return A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$ariaProperty, aps);
			}
		case 1:
			var b = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'disable', b);
		case 2:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'clipHeight', x);
		case 3:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'columnPadding', x);
		case 34:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'rowPadding', x);
		case 4:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'columns', x);
		case 6:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'cornerRadius', x);
		case 5:
			var d = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'direction',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$markOrientationLabel(d)))
				]);
		case 7:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'fillColor', s);
		case 32:
			var orient = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'orient',
					$gicentre$elm_vegalite$VegaLite$legendOrientSpec(orient))
				]);
		case 31:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'offset', x);
		case 35:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'strokeColor', s);
		case 36:
			var xs = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'strokeDash', xs);
		case 37:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeWidth', x);
		case 33:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'padding', x);
		case 8:
			var d = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'gradientDirection',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$markOrientationLabel(d)))
				]);
		case 16:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientHorizontalMaxLength', x);
		case 17:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientHorizontalMinLength', x);
		case 18:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientVerticalMaxLength', x);
		case 19:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientVerticalMinLength', x);
		case 9:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientLabelLimit', x);
		case 11:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientLength', x);
		case 10:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientLabelOffset', x);
		case 12:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientOpacity', x);
		case 13:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'gradientStrokeColor', s);
		case 14:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientStrokeWidth', x);
		case 15:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'gradientThickness', x);
		case 20:
			var ga = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'gridAlign',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$compositionAlignmentLabel(ga)))
				]);
		case 21:
			var ha = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelAlign',
					$gicentre$elm_vegalite$VegaLite$hAlignSpec(ha))
				]);
		case 22:
			var va = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelBaseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 23:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelColor', s);
		case 24:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelFont', s);
		case 25:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelFontSize', x);
		case 26:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'labelFontStyle', s);
		case 27:
			var fw = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelFontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw))
				]);
		case 28:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelLimit', x);
		case 29:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'labelOffset', x);
		case 30:
			var lo = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'labelOverlap',
					$gicentre$elm_vegalite$VegaLite$overlapStrategySpec(lo))
				]);
		case 42:
			var d = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'symbolDirection',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$markOrientationLabel(d)))
				]);
		case 44:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolLimit', x);
		case 43:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'symbolFillColor', s);
		case 38:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'symbolBaseFillColor', s);
		case 50:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'symbolStrokeColor', s);
		case 39:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'symbolBaseStrokeColor', s);
		case 40:
			var xs = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'symbolDash', xs);
		case 41:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolDashOffset', x);
		case 45:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolOffset', x);
		case 46:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolOpacity', x);
		case 47:
			var s = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'symbolType',
					$gicentre$elm_vegalite$VegaLite$symbolSpec(s))
				]);
		case 48:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolSize', x);
		case 49:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'symbolStrokeWidth', x);
		case 51:
			return _List_fromArray(
				[
					_Utils_Tuple2('title', $elm$json$Json$Encode$null)
				]);
		case 52:
			var ha = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleAlign',
					$gicentre$elm_vegalite$VegaLite$hAlignSpec(ha))
				]);
		case 54:
			var va = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleBaseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 53:
			var an = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleAnchor',
					$gicentre$elm_vegalite$VegaLite$anchorSpec(an))
				]);
		case 55:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleColor', s);
		case 56:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleFont', s);
		case 57:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleFontSize', x);
		case 58:
			var s = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'titleFontStyle', s);
		case 59:
			var fw = legendConfig.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'titleFontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw))
				]);
		case 60:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleLimit', x);
		case 61:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleLineHeight', x);
		case 62:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titleOpacity', x);
		case 63:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'titlePadding', x);
		case 64:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'unselectedOpacity', x);
		case 65:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'legendX', x);
		default:
			var x = legendConfig.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'legendY', x);
	}
};
var $gicentre$elm_vegalite$VegaLite$localeProperty = function (lp) {
	switch (lp.$) {
		case 0:
			var s = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'decimal', s);
		case 1:
			var s = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'thousands', s);
		case 2:
			var grp = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'grouping', grp);
		case 3:
			var ss = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'currency', ss);
		case 4:
			var ss = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'numerals', ss);
		case 5:
			var s = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'percent', s);
		case 6:
			var s = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'minus', s);
		case 7:
			var s = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'nan', s);
		case 8:
			var s = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'dateTime', s);
		case 9:
			var s = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'date', s);
		case 10:
			var s = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'time', s);
		case 11:
			var ps = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'periods', ps);
		case 12:
			var ss = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'days', ss);
		case 13:
			var ss = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'shortDays', ss);
		case 14:
			var ss = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'months', ss);
		default:
			var ss = lp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'shortMonths', ss);
	}
};
var $elm$core$Tuple$mapBoth = F3(
	function (funcA, funcB, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			funcA(x),
			funcB(y));
	});
var $elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _v0) {
				var trues = _v0.a;
				var falses = _v0.b;
				return pred(x) ? _Utils_Tuple2(
					A2($elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2($elm$core$List$cons, x, falses));
			});
		return A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var $gicentre$elm_vegalite$VegaLite$localeProperties = function (lps) {
	var splitNumDate = function (lp) {
		switch (lp.$) {
			case 0:
				return true;
			case 1:
				return true;
			case 2:
				return true;
			case 3:
				return true;
			case 4:
				return true;
			case 5:
				return true;
			case 6:
				return true;
			case 7:
				return true;
			default:
				return false;
		}
	};
	return A3(
		$elm$core$Tuple$mapBoth,
		$elm$core$List$concatMap($gicentre$elm_vegalite$VegaLite$localeProperty),
		$elm$core$List$concatMap($gicentre$elm_vegalite$VegaLite$localeProperty),
		A2($elm$core$List$partition, splitNumDate, lps));
};
var $gicentre$elm_vegalite$VegaLite$paddingSpec = function (pad) {
	switch (pad.$) {
		case 0:
			var x = pad.a;
			switch (x.$) {
				case 0:
					var n = x.a;
					return $elm$json$Json$Encode$float(n);
				case 1:
					return $elm$json$Json$Encode$null;
				default:
					var s = x.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(s))
							]));
			}
		case 1:
			var s = pad.a;
			if (!s.$) {
				var ns = s.a;
				if ((((ns.b && ns.b.b) && ns.b.b.b) && ns.b.b.b.b) && (!ns.b.b.b.b.b)) {
					var l = ns.a;
					var _v4 = ns.b;
					var t = _v4.a;
					var _v5 = _v4.b;
					var r = _v5.a;
					var _v6 = _v5.b;
					var b = _v6.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'left',
								$elm$json$Json$Encode$float(l)),
								_Utils_Tuple2(
								'top',
								$elm$json$Json$Encode$float(t)),
								_Utils_Tuple2(
								'right',
								$elm$json$Json$Encode$float(r)),
								_Utils_Tuple2(
								'bottom',
								$elm$json$Json$Encode$float(b))
							]));
				} else {
					return $elm$json$Json$Encode$null;
				}
			} else {
				var ee = s.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'expr',
							$elm$json$Json$Encode$string(ee))
						]));
			}
		default:
			var s = pad.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $gicentre$elm_vegalite$VegaLite$numStr = function (n) {
	switch (n.$) {
		case 0:
			var x = n.a;
			return $elm$core$String$fromFloat(x);
		case 1:
			return 'null';
		default:
			var s = n.a;
			return s;
	}
};
var $gicentre$elm_vegalite$VegaLite$projectionSpec = function (proj) {
	switch (proj.$) {
		case 0:
			return $elm$json$Json$Encode$string('albers');
		case 1:
			return $elm$json$Json$Encode$string('albersUsa');
		case 2:
			return $elm$json$Json$Encode$string('azimuthalEqualArea');
		case 3:
			return $elm$json$Json$Encode$string('azimuthalEquidistant');
		case 4:
			return $elm$json$Json$Encode$string('conicConformal');
		case 5:
			return $elm$json$Json$Encode$string('conicEqualarea');
		case 6:
			return $elm$json$Json$Encode$string('conicEquidistant');
		case 7:
			var projName = proj.a;
			return $elm$json$Json$Encode$string(projName);
		case 9:
			return $elm$json$Json$Encode$string('equalEarth');
		case 8:
			return $elm$json$Json$Encode$string('equirectangular');
		case 10:
			return $elm$json$Json$Encode$string('gnomonic');
		case 11:
			return $elm$json$Json$Encode$string('identity');
		case 12:
			return $elm$json$Json$Encode$string('mercator');
		case 13:
			return $elm$json$Json$Encode$string('naturalEarth1');
		case 14:
			return $elm$json$Json$Encode$string('orthographic');
		case 15:
			return $elm$json$Json$Encode$string('stereographic');
		case 16:
			return $elm$json$Json$Encode$string('transverseMercator');
		default:
			var s = proj.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$projectionProperty = function (pp) {
	switch (pp.$) {
		case 0:
			var proj = pp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$gicentre$elm_vegalite$VegaLite$projectionSpec(proj))
				]);
		case 6:
			var spec = pp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2('fit', spec)
				]);
		case 1:
			var n = pp.a;
			switch (n.$) {
				case 0:
					var x = n.a;
					return (x > 0) ? A2($gicentre$elm_vegalite$VegaLite$numExpr, 'clipAngle', n) : _List_fromArray(
						[
							_Utils_Tuple2('clipAngle', $elm$json$Json$Encode$null)
						]);
				case 1:
					return _List_fromArray(
						[
							_Utils_Tuple2('clipAngle', $elm$json$Json$Encode$null)
						]);
				default:
					return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'clipAngle', n);
			}
		case 2:
			var rClip = pp.a;
			switch (rClip.$) {
				case 0:
					return _List_fromArray(
						[
							_Utils_Tuple2('clipExtent', $elm$json$Json$Encode$null)
						]);
				case 1:
					var l = rClip.a;
					var t = rClip.b;
					var r = rClip.c;
					var b = rClip.d;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'clipExtent',
							A2(
								$elm$json$Json$Encode$list,
								$elm$json$Json$Encode$list($elm$json$Json$Encode$float),
								_List_fromArray(
									[
										_List_fromArray(
										[l, t]),
										_List_fromArray(
										[r, b])
									])))
						]);
				default:
					var l = rClip.a;
					var t = rClip.b;
					var r = rClip.c;
					var b = rClip.d;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'clipExtent',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'expr',
										$elm$json$Json$Encode$string('[[' + (l + (',' + (t + ('],[' + (r + (',' + (b + ']]')))))))))
									])))
						]);
			}
		case 11:
			var b = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'reflectX', b);
		case 12:
			var b = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'reflectY', b);
		case 3:
			var lambda = pp.a;
			var phi = pp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'center',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(
									'[' + ($gicentre$elm_vegalite$VegaLite$numStr(lambda) + (',' + ($gicentre$elm_vegalite$VegaLite$numStr(phi) + ']')))))
							])))
				]);
		case 4:
			var x = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'scale', x);
		case 5:
			var tx = pp.a;
			var ty = pp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'translate',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(
									'[' + ($gicentre$elm_vegalite$VegaLite$numStr(tx) + (',' + ($gicentre$elm_vegalite$VegaLite$numStr(ty) + ']')))))
							])))
				]);
		case 7:
			var lambda = pp.a;
			var phi = pp.b;
			var gamma = pp.c;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'rotate',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(
									'[' + ($gicentre$elm_vegalite$VegaLite$numStr(lambda) + (',' + ($gicentre$elm_vegalite$VegaLite$numStr(phi) + (',' + ($gicentre$elm_vegalite$VegaLite$numStr(gamma) + ']')))))))
							])))
				]);
		case 10:
			var x = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'pointRadius', x);
		case 8:
			var x = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'precision', x);
		case 13:
			var x = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'coefficient', x);
		case 14:
			var x = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'distance', x);
		case 15:
			var x = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'fraction', x);
		case 16:
			var n = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'lobes', n);
		case 17:
			var lat = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'parallel', lat);
		case 9:
			var lat1 = pp.a;
			var lat2 = pp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'parallels',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expr',
								$elm$json$Json$Encode$string(
									'[' + ($gicentre$elm_vegalite$VegaLite$numStr(lat1) + (',' + ($gicentre$elm_vegalite$VegaLite$numStr(lat2) + ']')))))
							])))
				]);
		case 18:
			var x = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'radius', x);
		case 19:
			var x = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'ratio', x);
		case 20:
			var x = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'spacing', x);
		default:
			var x = pp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'tilt', x);
	}
};
var $gicentre$elm_vegalite$VegaLite$rangeConfigProperty = function (rangeCfg) {
	switch (rangeCfg.$) {
		case 0:
			var clrs = rangeCfg.a;
			return _Utils_Tuple2(
				'category',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							A2($gicentre$elm_vegalite$VegaLite$schemeProperty, clrs, _List_Nil)
						])));
		case 1:
			var clrs = rangeCfg.a;
			return _Utils_Tuple2(
				'diverging',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							A2($gicentre$elm_vegalite$VegaLite$schemeProperty, clrs, _List_Nil)
						])));
		case 2:
			var clrs = rangeCfg.a;
			return _Utils_Tuple2(
				'heatmap',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							A2($gicentre$elm_vegalite$VegaLite$schemeProperty, clrs, _List_Nil)
						])));
		case 3:
			var clrs = rangeCfg.a;
			return _Utils_Tuple2(
				'ordinal',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							A2($gicentre$elm_vegalite$VegaLite$schemeProperty, clrs, _List_Nil)
						])));
		case 4:
			var clrs = rangeCfg.a;
			return _Utils_Tuple2(
				'ramp',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							A2($gicentre$elm_vegalite$VegaLite$schemeProperty, clrs, _List_Nil)
						])));
		case 5:
			var syms = rangeCfg.a;
			return _Utils_Tuple2(
				'symbol',
				A2($elm$json$Json$Encode$list, $gicentre$elm_vegalite$VegaLite$symbolSpec, syms));
		default:
			var s = rangeCfg.a;
			return _Utils_Tuple2(
				'symbol',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'signal',
							$elm$json$Json$Encode$string(s))
						])));
	}
};
var $gicentre$elm_vegalite$VegaLite$scaleConfigProperty = function (scaleCfg) {
	switch (scaleCfg.$) {
		case 0:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'bandPaddingInner', x);
		case 3:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'bandPaddingOuter', x);
		case 1:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'barBandPaddingInner', x);
		case 2:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'rectBandPaddingInner', x);
		case 5:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'continuousPadding', x);
		case 4:
			var b = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'clamp', b);
		case 6:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'maxBandSize', x);
		case 7:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'minBandSize', x);
		case 8:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'maxFontSize', x);
		case 9:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'minFontSize', x);
		case 10:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'maxOpacity', x);
		case 11:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'minOpacity', x);
		case 12:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'maxSize', x);
		case 13:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'minSize', x);
		case 14:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'maxStrokeWidth', x);
		case 15:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'minStrokeWidth', x);
		case 16:
			var x = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'pointPadding', x);
		case 17:
			var b = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'round', b);
		case 18:
			var b = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'useUnaggregatedDomain', b);
		case 19:
			var b = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'xReverse', b);
		default:
			var b = scaleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'zero', b);
	}
};
var $gicentre$elm_vegalite$VegaLite$selectionLabel = function (seType) {
	if (!seType) {
		return 'point';
	} else {
		return 'interval';
	}
};
var $gicentre$elm_vegalite$VegaLite$selectionMarkProperty = function (markProp) {
	switch (markProp.$) {
		case 0:
			var ss = markProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'fill', ss);
		case 1:
			var x = markProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'fillOpacity', x);
		case 2:
			var s = markProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'stroke', s);
		case 3:
			var x = markProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeOpacity', x);
		case 4:
			var x = markProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeWidth', x);
		case 5:
			var xs = markProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'strokeDash', xs);
		case 6:
			var x = markProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeDashOffset', x);
		default:
			var cur = markProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'cursor',
					$gicentre$elm_vegalite$VegaLite$cursorSpec(cur))
				]);
	}
};
var $gicentre$elm_vegalite$VegaLite$selectionResolutionLabel = function (res) {
	switch (res) {
		case 0:
			return 'global';
		case 1:
			return 'union';
		default:
			return 'intersect';
	}
};
var $gicentre$elm_vegalite$VegaLite$togglePredicateSpec = function (tp) {
	switch (tp.$) {
		case 0:
			return $elm$json$Json$Encode$bool(false);
		case 1:
			var ex = tp.a;
			return $elm$json$Json$Encode$string(ex);
		case 2:
			return $elm$json$Json$Encode$string('event.shiftKey');
		case 3:
			return $elm$json$Json$Encode$string('event.ctrlKey');
		default:
			return $elm$json$Json$Encode$string('event.altKey');
	}
};
var $gicentre$elm_vegalite$VegaLite$selectionProperties = function (selProp) {
	switch (selProp.$) {
		case 4:
			var fNames = selProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'fields',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, fNames))
				]);
		case 5:
			var channels = selProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'encodings',
					A2(
						$elm$json$Json$Encode$list,
						A2($elm$core$Basics$composeL, $elm$json$Json$Encode$string, $gicentre$elm_vegalite$VegaLite$channelLabel),
						channels))
				]);
		case 0:
			var s = selProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'on', s);
		case 1:
			var es = selProp.a;
			switch (es.$) {
				case 0:
					var s = es.a;
					return ($elm$core$String$trim(s) === '') ? _List_fromArray(
						[
							_Utils_Tuple2(
							'clear',
							$elm$json$Json$Encode$bool(false))
						]) : _List_fromArray(
						[
							_Utils_Tuple2(
							'clear',
							$elm$json$Json$Encode$string(
								$elm$core$String$trim(s)))
						]);
				case 1:
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'clear',
							$elm$json$Json$Encode$bool(false))
						]);
				default:
					return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'clear', es);
			}
		case 6:
			var res = selProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'resolve',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$selectionResolutionLabel(res)))
				]);
		case 7:
			var markProps = selProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'mark',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$selectionMarkProperty, markProps)))
				]);
		case 8:
			var b = selProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'nearest', b);
		case 9:
			var tp = selProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'toggle',
					$gicentre$elm_vegalite$VegaLite$togglePredicateSpec(tp))
				]);
		case 2:
			var es = selProp.a;
			switch (es.$) {
				case 0:
					var s = es.a;
					return ($elm$core$String$trim(s) === '') ? _List_fromArray(
						[
							_Utils_Tuple2(
							'translate',
							$elm$json$Json$Encode$bool(false))
						]) : _List_fromArray(
						[
							_Utils_Tuple2(
							'translate',
							$elm$json$Json$Encode$string(
								$elm$core$String$trim(s)))
						]);
				case 1:
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'translate',
							$elm$json$Json$Encode$bool(false))
						]);
				default:
					return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'translate', es);
			}
		default:
			var es = selProp.a;
			switch (es.$) {
				case 0:
					var s = es.a;
					return ($elm$core$String$trim(s) === '') ? _List_fromArray(
						[
							_Utils_Tuple2(
							'zoom',
							$elm$json$Json$Encode$bool(false))
						]) : _List_fromArray(
						[
							_Utils_Tuple2(
							'zoom',
							$elm$json$Json$Encode$string(
								$elm$core$String$trim(s)))
						]);
				case 1:
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'zoom',
							$elm$json$Json$Encode$bool(false))
						]);
				default:
					return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'zoom', es);
			}
	}
};
var $gicentre$elm_vegalite$VegaLite$tfSpec = function (tf) {
	switch (tf.$) {
		case 1:
			return $elm$json$Json$Encode$string('group');
		case 0:
			return $elm$json$Json$Encode$string('bounds');
		default:
			var s = tf.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'expr',
						$elm$json$Json$Encode$string(s))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$titleConfigProperty = function (titleCfg) {
	switch (titleCfg.$) {
		case 0:
			var an = titleCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'anchor',
					$gicentre$elm_vegalite$VegaLite$anchorSpec(an))
				]);
		case 1:
			var n = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'angle', n);
		case 2:
			var va = titleCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'baseline',
					$gicentre$elm_vegalite$VegaLite$vAlignSpec(va))
				]);
		case 3:
			var s = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'color', s);
		case 4:
			var n = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'dx', n);
		case 5:
			var n = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'dy', n);
		case 6:
			var s = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'font', s);
		case 7:
			var n = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'fontSize', n);
		case 8:
			var s = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'fontStyle', s);
		case 10:
			var tf = titleCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'frame',
					$gicentre$elm_vegalite$VegaLite$tfSpec(tf))
				]);
		case 9:
			var w = titleCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'fontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(w))
				]);
		case 12:
			var n = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'limit', n);
		case 11:
			var n = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'lineHeight', n);
		case 13:
			var n = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'offset', n);
		case 14:
			var sd = titleCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'orient',
					$gicentre$elm_vegalite$VegaLite$sideSpec(sd))
				]);
		case 15:
			var ss = titleCfg.a;
			if (ss.b && (!ss.b.b)) {
				var s = ss.a;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'style',
						$elm$json$Json$Encode$string(s))
					]);
			} else {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'style',
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, ss))
					]);
			}
		case 16:
			var s = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExprMultiline, 'subtitle', s);
		case 17:
			var s = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'subtitleColor', s);
		case 18:
			var s = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'subtitleFont', s);
		case 19:
			var n = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'subtitleFontSize', n);
		case 20:
			var s = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'subtitleFontStyle', s);
		case 21:
			var w = titleCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'subtitleFontWeight',
					$gicentre$elm_vegalite$VegaLite$fontWeightSpec(w))
				]);
		case 22:
			var n = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'subtitleLineHeight', n);
		case 23:
			var n = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'subtitlePadding', n);
		default:
			var n = titleCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'zindex', n);
	}
};
var $gicentre$elm_vegalite$VegaLite$viewBackgroundProperty = function (vb) {
	switch (vb.$) {
		case 0:
			var ss = vb.a;
			return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'style', ss);
		case 1:
			var n = vb.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'cornerRadius', n);
		case 2:
			var s = vb.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'fill', s);
		case 3:
			var n = vb.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'fillOpacity', n);
		case 4:
			var n = vb.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'opacity', n);
		case 5:
			var s = vb.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'stroke', s);
		case 6:
			var n = vb.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeOpacity', n);
		case 8:
			var cap = vb.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'strokeCap',
					$gicentre$elm_vegalite$VegaLite$strokeCapSpec(cap))
				]);
		case 11:
			var jn = vb.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'strokeJoin',
					$gicentre$elm_vegalite$VegaLite$strokeJoinSpec(jn))
				]);
		case 7:
			var n = vb.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeWidth', n);
		case 9:
			var ns = vb.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'strokeDash', ns);
		case 10:
			var n = vb.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeDashOffset', n);
		default:
			var n = vb.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeMiterLimit', n);
	}
};
var $gicentre$elm_vegalite$VegaLite$viewConfigProperties = function (viewCfg) {
	switch (viewCfg.$) {
		case 2:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'continuousWidth', x);
		case 3:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'continuousHeight', x);
		case 6:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'discreteWidth', x);
		case 7:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'discreteHeight', x);
		case 1:
			var b = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'clip', b);
		case 4:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'cornerRadius', x);
		case 5:
			var cur = viewCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'cursor',
					$gicentre$elm_vegalite$VegaLite$cursorSpec(cur))
				]);
		case 8:
			var s = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'fill', s);
		case 9:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'fillOpacity', x);
		case 10:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'opacity', x);
		case 11:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'step', x);
		case 12:
			var s = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'stroke', s);
		case 13:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeOpacity', x);
		case 15:
			var cap = viewCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'strokeCap',
					$gicentre$elm_vegalite$VegaLite$strokeCapSpec(cap))
				]);
		case 18:
			var jn = viewCfg.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'strokeJoin',
					$gicentre$elm_vegalite$VegaLite$strokeJoinSpec(jn))
				]);
		case 14:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeWidth', x);
		case 16:
			var xs = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numsExpr, 'strokeDash', xs);
		case 17:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeDashOffset', x);
		case 19:
			var x = viewCfg.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'strokeMiterLimit', x);
		default:
			var vbs = viewCfg.a;
			return A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$viewBackgroundProperty, vbs);
	}
};
var $gicentre$elm_vegalite$VegaLite$configProperty = function (configProp) {
	switch (configProp.$) {
		case 1:
			var b = configProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'aria', b);
		case 2:
			var aus = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'autosize',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$autosizeProperty, aus)))
				]);
		case 14:
			var s = configProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'background', s);
		case 18:
			var s = configProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'countTitle', s);
		case 24:
			var lps = configProp.a;
			var _v1 = $gicentre$elm_vegalite$VegaLite$localeProperties(lps);
			if (!_v1.a.b) {
				if (!_v1.b.b) {
					return _List_fromArray(
						[
							_Utils_Tuple2('locale', $elm$json$Json$Encode$null)
						]);
				} else {
					var dtps = _v1.b;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'locale',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'time',
										$elm$json$Json$Encode$object(dtps))
									])))
						]);
				}
			} else {
				if (!_v1.b.b) {
					var nps = _v1.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'locale',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'number',
										$elm$json$Json$Encode$object(nps))
									])))
						]);
				} else {
					var nps = _v1.a;
					var dtps = _v1.b;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'locale',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'number',
										$elm$json$Json$Encode$object(nps)),
										_Utils_Tuple2(
										'time',
										$elm$json$Json$Encode$object(dtps))
									])))
						]);
				}
			}
		case 20:
			var ftp = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'fieldTitle',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$fieldTitleLabel(ftp)))
				]);
		case 30:
			var s = configProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'normalizedNumberFormat', s);
		case 31:
			var s = configProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'normalizedNumberFormatType', s);
		case 32:
			var s = configProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'numberFormat', s);
		case 33:
			var s = configProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'numberFormatType', s);
		case 48:
			var cps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'tooltipFormat',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$configProperty, cps)))
				]);
		case 34:
			var pad = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'padding',
					$gicentre$elm_vegalite$VegaLite$paddingSpec(pad))
				]);
		case 46:
			var s = configProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'timeFormat', s);
		case 47:
			var s = configProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'timeFormatType', s);
		case 3:
			var axType = configProp.a;
			var acs = configProp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					$gicentre$elm_vegalite$VegaLite$axisLabel(axType),
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)))
				]);
		case 5:
			var acs = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'axisLeft',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)))
				]);
		case 6:
			var acs = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'axisRight',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)))
				]);
		case 7:
			var acs = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'axisTop',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)))
				]);
		case 8:
			var acs = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'axisBottom',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)))
				]);
		case 9:
			var axType = configProp.a;
			var acs = configProp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					$gicentre$elm_vegalite$VegaLite$axisLabel(axType) + 'Band',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)))
				]);
		case 10:
			var axType = configProp.a;
			var acs = configProp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					$gicentre$elm_vegalite$VegaLite$axisLabel(axType) + 'Discrete',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)))
				]);
		case 11:
			var axType = configProp.a;
			var acs = configProp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					$gicentre$elm_vegalite$VegaLite$axisLabel(axType) + 'Point',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)))
				]);
		case 12:
			var axType = configProp.a;
			var acs = configProp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					$gicentre$elm_vegalite$VegaLite$axisLabel(axType) + 'Quantitative',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)))
				]);
		case 13:
			var axType = configProp.a;
			var acs = configProp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					$gicentre$elm_vegalite$VegaLite$axisLabel(axType) + 'Temporal',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)))
				]);
		case 23:
			var lcs = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'legend',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$legendConfigProperty, lcs)))
				]);
		case 21:
			var s = configProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'font', s);
		case 28:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'mark',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 36:
			var pps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'projection',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$projectionProperty, pps)))
				]);
		case 0:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'area',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 15:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'bar',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 16:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'circle',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 26:
			var fps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'facet',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$facetConfigProperty, fps)))
				]);
		case 17:
			var cps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'concat',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$concatConfigProperty, cps)))
				]);
		case 19:
			var b = configProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'customFormatTypes', b);
		case 22:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'geoshape',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 27:
			var hps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'header',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$headerProperty, hps)))
				]);
		case 25:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'line',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 35:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'point',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 38:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'rect',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 39:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'rule',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 42:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'square',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 43:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'text',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 44:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'tick',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
		case 45:
			var tcs = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'title',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$titleConfigProperty, tcs)))
				]);
		case 29:
			var styles = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'style',
					$elm$json$Json$Encode$object(
						A2(
							$elm$core$List$map,
							function (_v2) {
								var sName = _v2.a;
								var mps = _v2.b;
								return _Utils_Tuple2(
									sName,
									$elm$json$Json$Encode$object(
										A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
							},
							styles)))
				]);
		case 4:
			var styles = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'style',
					$elm$json$Json$Encode$object(
						A2(
							$elm$core$List$map,
							function (_v3) {
								var sName = _v3.a;
								var mps = _v3.b;
								return _Utils_Tuple2(
									sName,
									$elm$json$Json$Encode$object(
										A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisProperty, mps)));
							},
							styles)))
				]);
		case 40:
			var scs = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'scale',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$scaleConfigProperty, scs)))
				]);
		case 37:
			var rcs = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'range',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$rangeConfigProperty, rcs)))
				]);
		case 41:
			var selConfig = configProp.a;
			var selProp = function (_v4) {
				var sel = _v4.a;
				var sps = _v4.b;
				return _Utils_Tuple2(
					$gicentre$elm_vegalite$VegaLite$selectionLabel(sel),
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$selectionProperties, sps)));
			};
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'selection',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$map, selProp, selConfig)))
				]);
		case 50:
			var vcs = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'view',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$viewConfigProperties, vcs)))
				]);
		default:
			var mps = configProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'trail',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
				]);
	}
};
var $gicentre$elm_vegalite$VegaLite$configuration = function (cfg) {
	return $elm$core$Basics$append(
		$gicentre$elm_vegalite$VegaLite$configProperty(cfg));
};
var $gicentre$elm_vegalite$VegaLite$VLConfig = 32;
var $gicentre$elm_vegalite$VegaLite$configure = function (configs) {
	return _Utils_Tuple2(
		32,
		$elm$json$Json$Encode$object(configs));
};
var $gicentre$elm_vegalite$VegaLite$dataColumn = F2(
	function (colName, data) {
		switch (data.$) {
			case 2:
				var col = data.a;
				return $elm$core$List$cons(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(
								colName,
								$elm$json$Json$Encode$float(x));
						},
						col));
			case 4:
				var col = data.a;
				return $elm$core$List$cons(
					A2(
						$elm$core$List$map,
						function (s) {
							return _Utils_Tuple2(
								colName,
								$elm$json$Json$Encode$string(s));
						},
						col));
			case 1:
				var col = data.a;
				return $elm$core$List$cons(
					A2(
						$elm$core$List$map,
						function (ds) {
							return _Utils_Tuple2(
								colName,
								$elm$json$Json$Encode$object(
									A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, ds)));
						},
						col));
			case 0:
				var col = data.a;
				return $elm$core$List$cons(
					A2(
						$elm$core$List$map,
						function (b) {
							return _Utils_Tuple2(
								colName,
								$elm$json$Json$Encode$bool(b));
						},
						col));
			case 3:
				var col = data.a;
				return $elm$core$List$cons(
					_List_fromArray(
						[
							_Utils_Tuple2(
							colName,
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'expr',
										$elm$json$Json$Encode$string(col))
									])))
						]));
			case 5:
				var col = data.a;
				return $elm$core$List$cons(
					A2(
						$elm$core$List$map,
						function (kvs) {
							return _Utils_Tuple2(
								colName,
								$elm$json$Json$Encode$object(
									A2(
										$elm$core$List$map,
										function (_v1) {
											var k = _v1.a;
											var v = _v1.b;
											return _Utils_Tuple2(
												k,
												$gicentre$elm_vegalite$VegaLite$dataValueSpec(v));
										},
										kvs)));
						},
						col));
			default:
				var col = data.a;
				return $elm$core$List$cons(
					A2(
						$elm$core$List$map,
						function (ds) {
							return _Utils_Tuple2(
								colName,
								$gicentre$elm_vegalite$VegaLite$dataValuesSpecs(ds));
						},
						col));
		}
	});
var $gicentre$elm_vegalite$VegaLite$VLData = 13;
var $gicentre$elm_vegalite$VegaLite$dataTypeLabel = function (dType) {
	switch (dType.$) {
		case 0:
			return 'number';
		case 1:
			return 'boolean';
		case 2:
			var dateFmt = dType.a;
			return (dateFmt === '') ? 'date' : ('date:\'' + (dateFmt + '\''));
		default:
			var dateFmt = dType.a;
			return (dateFmt === '') ? 'utc' : ('utc:\'' + (dateFmt + '\''));
	}
};
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $gicentre$elm_vegalite$VegaLite$formatProperties = function (fmt) {
	switch (fmt.$) {
		case 0:
			var s = fmt.a;
			switch (s.$) {
				case 0:
					var propertyName = s.a;
					return ($elm$core$String$trim(propertyName) === '') ? _List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string('json'))
						]) : _List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string('json')),
							_Utils_Tuple2(
							'property',
							$elm$json$Json$Encode$string(propertyName))
						]);
				case 1:
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string('json'))
						]);
				default:
					var st = s.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'expr',
										$elm$json$Json$Encode$string(st))
									])))
						]);
			}
		case 1:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('csv'))
				]);
		case 2:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('tsv'))
				]);
		case 3:
			var delim = fmt.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('dsv')),
					_Utils_Tuple2(
					'delimiter',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromChar(delim)))
				]);
		case 4:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('arrow'))
				]);
		case 5:
			var s = fmt.a;
			return A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('topojson')),
				A2($gicentre$elm_vegalite$VegaLite$strExpr, 'feature', s));
		case 6:
			var s = fmt.a;
			return A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('topojson')),
				A2($gicentre$elm_vegalite$VegaLite$strExpr, 'mesh', s));
		default:
			var fmts = fmt.a;
			return _Utils_eq(fmts, _List_Nil) ? _List_fromArray(
				[
					_Utils_Tuple2('parse', $elm$json$Json$Encode$null)
				]) : _List_fromArray(
				[
					_Utils_Tuple2(
					'parse',
					$elm$json$Json$Encode$object(
						A2(
							$elm$core$List$map,
							function (_v2) {
								var field = _v2.a;
								var fFormat = _v2.b;
								return _Utils_Tuple2(
									field,
									$elm$json$Json$Encode$string(
										$gicentre$elm_vegalite$VegaLite$dataTypeLabel(fFormat)));
							},
							fmts)))
				]);
	}
};
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $gicentre$elm_vegalite$VegaLite$transpose = function (xss) {
	var numCols = A2(
		$elm$core$Basics$composeR,
		$elm$core$List$head,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Maybe$withDefault(_List_Nil),
			$elm$core$List$length));
	return A3(
		$elm$core$List$foldr,
		$elm$core$List$map2($elm$core$List$cons),
		A2(
			$elm$core$List$repeat,
			numCols(xss),
			_List_Nil),
		xss);
};
var $gicentre$elm_vegalite$VegaLite$dataFromColumns = F2(
	function (fmts, cols) {
		if (_Utils_eq(cols, _List_Nil)) {
			return _Utils_Tuple2(
				13,
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'values',
							$gicentre$elm_vegalite$VegaLite$toList(
								_List_fromArray(
									[
										$elm$json$Json$Encode$object(_List_Nil)
									])))
						])));
		} else {
			var dArray = A2(
				$elm$json$Json$Encode$list,
				$elm$json$Json$Encode$object,
				$gicentre$elm_vegalite$VegaLite$transpose(cols));
			return _Utils_eq(fmts, _List_Nil) ? _Utils_Tuple2(
				13,
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2('values', dArray)
						]))) : _Utils_Tuple2(
				13,
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2('values', dArray),
							_Utils_Tuple2(
							'format',
							$elm$json$Json$Encode$object(
								A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$formatProperties, fmts)))
						])));
		}
	});
var $gicentre$elm_vegalite$VegaLite$VLEncoding = 18;
var $gicentre$elm_vegalite$VegaLite$encoding = function (channels) {
	return _Utils_Tuple2(
		18,
		$elm$json$Json$Encode$object(channels));
};
var $gicentre$elm_vegalite$VegaLite$sortFieldProperties = function (wsf) {
	if (!wsf.$) {
		var f = wsf.a;
		return _List_fromArray(
			[
				_Utils_Tuple2(
				'field',
				$elm$json$Json$Encode$string(f)),
				_Utils_Tuple2(
				'order',
				$elm$json$Json$Encode$string('ascending'))
			]);
	} else {
		var f = wsf.a;
		return _List_fromArray(
			[
				_Utils_Tuple2(
				'field',
				$elm$json$Json$Encode$string(f)),
				_Utils_Tuple2(
				'order',
				$elm$json$Json$Encode$string('descending'))
			]);
	}
};
var $gicentre$elm_vegalite$VegaLite$windowProperty = function (wProp) {
	var toNum = function (mn) {
		switch (mn.$) {
			case 1:
				return $elm$json$Json$Encode$null;
			case 0:
				var n = mn.a;
				return $elm$json$Json$Encode$float(n);
			default:
				var s = mn.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'expr',
							$elm$json$Json$Encode$string(s))
						]));
		}
	};
	switch (wProp.$) {
		case 0:
			var n1 = wProp.a;
			var n2 = wProp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'frame',
					$gicentre$elm_vegalite$VegaLite$toList(
						_List_fromArray(
							[
								toNum(n1),
								toNum(n2)
							])))
				]);
		case 1:
			var b = wProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$booExpr, 'ignorePeers', b);
		case 2:
			var ss = wProp.a;
			return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'groupby', ss);
		default:
			var sfs = wProp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'sort',
					A2(
						$elm$json$Json$Encode$list,
						A2($elm$core$Basics$composeR, $gicentre$elm_vegalite$VegaLite$sortFieldProperties, $elm$json$Json$Encode$object),
						sfs))
				]);
	}
};
var $gicentre$elm_vegalite$VegaLite$joinAggregate = F2(
	function (ops, wProps) {
		return $elm$core$List$cons(
			_Utils_Tuple2(
				'multiSpecs',
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'joinaggregate',
							$gicentre$elm_vegalite$VegaLite$toList(ops)),
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$windowProperty, wProps)))));
	});
var $gicentre$elm_vegalite$VegaLite$VLLayer = 19;
var $gicentre$elm_vegalite$VegaLite$layer = function (specs) {
	return _Utils_Tuple2(
		19,
		$gicentre$elm_vegalite$VegaLite$toList(specs));
};
var $gicentre$elm_vegalite$VegaLite$MLegend = function (a) {
	return {$: 15, a: a};
};
var $gicentre$elm_vegalite$VegaLite$mLegend = $gicentre$elm_vegalite$VegaLite$MLegend;
var $gicentre$elm_vegalite$VegaLite$MName = function (a) {
	return {$: 0, a: a};
};
var $gicentre$elm_vegalite$VegaLite$mName = function (s) {
	return $gicentre$elm_vegalite$VegaLite$MName(
		$gicentre$elm_vegalite$VegaLite$Str(s));
};
var $gicentre$elm_vegalite$VegaLite$MmType = function (a) {
	return {$: 6, a: a};
};
var $gicentre$elm_vegalite$VegaLite$Nominal = 0;
var $gicentre$elm_vegalite$VegaLite$mNominal = $gicentre$elm_vegalite$VegaLite$MmType(0);
var $gicentre$elm_vegalite$VegaLite$Number = function (a) {
	return {$: 2, a: a};
};
var $gicentre$elm_vegalite$VegaLite$num = $gicentre$elm_vegalite$VegaLite$Number;
var $gicentre$elm_vegalite$VegaLite$Numbers = function (a) {
	return {$: 2, a: a};
};
var $gicentre$elm_vegalite$VegaLite$nums = $gicentre$elm_vegalite$VegaLite$Numbers;
var $gicentre$elm_vegalite$VegaLite$opAs = F3(
	function (op, field, label) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'op',
					$gicentre$elm_vegalite$VegaLite$operationSpec(op)),
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(field)),
					_Utils_Tuple2(
					'as',
					$elm$json$Json$Encode$string(label))
				]));
	});
var $gicentre$elm_vegalite$VegaLite$Mean = {$: 7};
var $gicentre$elm_vegalite$VegaLite$opMean = $gicentre$elm_vegalite$VegaLite$Mean;
var $gicentre$elm_vegalite$VegaLite$PAggregate = function (a) {
	return {$: 12, a: a};
};
var $gicentre$elm_vegalite$VegaLite$pAggregate = $gicentre$elm_vegalite$VegaLite$PAggregate;
var $gicentre$elm_vegalite$VegaLite$PAxis = function (a) {
	return {$: 14, a: a};
};
var $gicentre$elm_vegalite$VegaLite$pAxis = $gicentre$elm_vegalite$VegaLite$PAxis;
var $gicentre$elm_vegalite$VegaLite$PDatum = function (a) {
	return {$: 1, a: a};
};
var $gicentre$elm_vegalite$VegaLite$pDatum = $gicentre$elm_vegalite$VegaLite$PDatum;
var $gicentre$elm_vegalite$VegaLite$PName = function (a) {
	return {$: 0, a: a};
};
var $gicentre$elm_vegalite$VegaLite$pName = function (s) {
	return $gicentre$elm_vegalite$VegaLite$PName(
		$gicentre$elm_vegalite$VegaLite$Str(s));
};
var $gicentre$elm_vegalite$VegaLite$PmType = function (a) {
	return {$: 7, a: a};
};
var $gicentre$elm_vegalite$VegaLite$Quantitative = 2;
var $gicentre$elm_vegalite$VegaLite$pQuant = $gicentre$elm_vegalite$VegaLite$PmType(2);
var $gicentre$elm_vegalite$VegaLite$imMethodLabel = function (method) {
	switch (method) {
		case 0:
			return 'value';
		case 1:
			return 'mean';
		case 2:
			return 'median';
		case 3:
			return 'max';
		default:
			return 'min';
	}
};
var $gicentre$elm_vegalite$VegaLite$imputeProperty = function (ip) {
	switch (ip.$) {
		case 0:
			if (!ip.a.$) {
				if (!ip.b.$) {
					var n1 = ip.a.a;
					var n2 = ip.b.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'frame',
							A2(
								$elm$json$Json$Encode$list,
								$elm$json$Json$Encode$int,
								_List_fromArray(
									[n1, n2])))
						]);
				} else {
					var n1 = ip.a.a;
					var _v2 = ip.b;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'frame',
							$gicentre$elm_vegalite$VegaLite$toList(
								_List_fromArray(
									[
										$elm$json$Json$Encode$int(n1),
										$elm$json$Json$Encode$null
									])))
						]);
				}
			} else {
				if (!ip.b.$) {
					var _v1 = ip.a;
					var n2 = ip.b.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'frame',
							$gicentre$elm_vegalite$VegaLite$toList(
								_List_fromArray(
									[
										$elm$json$Json$Encode$null,
										$elm$json$Json$Encode$int(n2)
									])))
						]);
				} else {
					var _v3 = ip.a;
					var _v4 = ip.b;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'frame',
							$gicentre$elm_vegalite$VegaLite$toList(
								_List_fromArray(
									[$elm$json$Json$Encode$null, $elm$json$Json$Encode$null])))
						]);
				}
			}
		case 1:
			var dVals = ip.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'keyvals',
					$gicentre$elm_vegalite$VegaLite$dataValuesSpecs(dVals))
				]);
		case 2:
			var start = ip.a;
			var stop = ip.b;
			var step = ip.c;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'keyvals',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'start',
								$elm$json$Json$Encode$float(start)),
								_Utils_Tuple2(
								'stop',
								$elm$json$Json$Encode$float(stop)),
								_Utils_Tuple2(
								'step',
								$elm$json$Json$Encode$float(step))
							])))
				]);
		case 3:
			var method = ip.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'method',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$imMethodLabel(method)))
				]);
		case 5:
			var dVal = ip.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'value',
					$gicentre$elm_vegalite$VegaLite$dataValueSpec(dVal))
				]);
		default:
			var ss = ip.a;
			return A2($gicentre$elm_vegalite$VegaLite$strsExpr, 'groupby', ss);
	}
};
var $gicentre$elm_vegalite$VegaLite$stackOffsetSpec = function (sp) {
	switch (sp) {
		case 0:
			return $elm$json$Json$Encode$string('zero');
		case 1:
			return $elm$json$Json$Encode$string('normalize');
		case 2:
			return $elm$json$Json$Encode$string('center');
		default:
			return $elm$json$Json$Encode$null;
	}
};
var $gicentre$elm_vegalite$VegaLite$stackOffsetProperty = function (offset) {
	return _Utils_Tuple2(
		'stack',
		$gicentre$elm_vegalite$VegaLite$stackOffsetSpec(offset));
};
var $gicentre$elm_vegalite$VegaLite$positionChannelProperty = function (pDef) {
	switch (pDef.$) {
		case 0:
			var s = pDef.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExpr, 'field', s);
		case 1:
			var d = pDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'datum',
					$gicentre$elm_vegalite$VegaLite$dataValueSpec(d))
				]);
		case 7:
			var measure = pDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$measurementLabel(measure)))
				]);
		case 8:
			var bps = pDef.a;
			return _List_fromArray(
				[
					$gicentre$elm_vegalite$VegaLite$bin(bps)
				]);
		case 9:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'bin',
					$elm$json$Json$Encode$string('binned'))
				]);
		case 12:
			var op = pDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'aggregate',
					$gicentre$elm_vegalite$VegaLite$operationSpec(op))
				]);
		case 10:
			var tu = pDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'timeUnit',
					$gicentre$elm_vegalite$VegaLite$timeUnitSpec(tu))
				]);
		case 11:
			var s = pDef.a;
			return A2($gicentre$elm_vegalite$VegaLite$strExprMultiline, 'title', s);
		case 15:
			var sps = pDef.a;
			_v1$4:
			while (true) {
				if (!sps.b) {
					return _List_fromArray(
						[
							_Utils_Tuple2('sort', $elm$json$Json$Encode$null)
						]);
				} else {
					if (!sps.b.b) {
						switch (sps.a.$) {
							case 0:
								var _v2 = sps.a;
								return _List_fromArray(
									[
										_Utils_Tuple2(
										'sort',
										$elm$json$Json$Encode$string('ascending'))
									]);
							case 1:
								var _v3 = sps.a;
								return _List_fromArray(
									[
										_Utils_Tuple2(
										'sort',
										$elm$json$Json$Encode$string('descending'))
									]);
							case 2:
								var dvs = sps.a.a;
								return _List_fromArray(
									[
										_Utils_Tuple2(
										'sort',
										$gicentre$elm_vegalite$VegaLite$dataValuesSpecs(dvs))
									]);
							default:
								break _v1$4;
						}
					} else {
						break _v1$4;
					}
				}
			}
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'sort',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$sortProperties, sps)))
				]);
		case 16:
			var x = pDef.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'bandPosition', x);
		case 13:
			var sps = pDef.a;
			return _Utils_eq(sps, _List_Nil) ? _List_fromArray(
				[
					_Utils_Tuple2('scale', $elm$json$Json$Encode$null)
				]) : _List_fromArray(
				[
					_Utils_Tuple2(
					'scale',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$scaleProperty, sps)))
				]);
		case 14:
			var aps = pDef.a;
			return _Utils_eq(aps, _List_Nil) ? _List_fromArray(
				[
					_Utils_Tuple2('axis', $elm$json$Json$Encode$null)
				]) : _List_fromArray(
				[
					_Utils_Tuple2(
					'axis',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$axisProperty, aps)))
				]);
		case 17:
			var so = pDef.a;
			return _List_fromArray(
				[
					$gicentre$elm_vegalite$VegaLite$stackOffsetProperty(so)
				]);
		case 5:
			var arr = pDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'repeat',
								$elm$json$Json$Encode$string(
									$gicentre$elm_vegalite$VegaLite$arrangementLabel(arr)))
							])))
				]);
		case 6:
			var arr = pDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'datum',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'repeat',
								$elm$json$Json$Encode$string(
									$gicentre$elm_vegalite$VegaLite$arrangementLabel(arr)))
							])))
				]);
		case 2:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'value',
					$elm$json$Json$Encode$string('width'))
				]);
		case 3:
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'value',
					$elm$json$Json$Encode$string('height'))
				]);
		case 4:
			var x = pDef.a;
			return A2($gicentre$elm_vegalite$VegaLite$numExpr, 'value', x);
		default:
			var ips = pDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'impute',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$imputeProperty, ips)))
				]);
	}
};
var $gicentre$elm_vegalite$VegaLite$positionLabel = function (pChannel) {
	switch (pChannel) {
		case 0:
			return 'x';
		case 1:
			return 'y';
		case 2:
			return 'x2';
		case 3:
			return 'y2';
		case 4:
			return 'xOffset';
		case 5:
			return 'yOffset';
		case 6:
			return 'theta';
		case 7:
			return 'theta2';
		case 8:
			return 'radius';
		case 9:
			return 'radius2';
		case 14:
			return 'xError';
		case 15:
			return 'yError';
		case 16:
			return 'xError2';
		case 17:
			return 'yError2';
		case 10:
			return 'longitude';
		case 11:
			return 'latitude';
		case 12:
			return 'longitude2';
		default:
			return 'latitude2';
	}
};
var $gicentre$elm_vegalite$VegaLite$position = F2(
	function (pos, pDefs) {
		return $elm$core$List$cons(
			_Utils_Tuple2(
				$gicentre$elm_vegalite$VegaLite$positionLabel(pos),
				$elm$json$Json$Encode$object(
					A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$positionChannelProperty, pDefs))));
	});
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Basics$modBy = _Basics_modBy;
var $author$project$Causality$positionInGroupInner = F3(
	function (countsSoFar, xValues, yValues) {
		var _v0 = _Utils_Tuple2(xValues, yValues);
		if (_v0.a.b && _v0.b.b) {
			var _v1 = _v0.a;
			var valX = _v1.a;
			var xRest = _v1.b;
			var _v2 = _v0.b;
			var valY = _v2.a;
			var yRest = _v2.b;
			var _v3 = valX ? (valY ? _Utils_Tuple2(
				function ($) {
					return $.y;
				},
				_Utils_update(
					countsSoFar,
					{y: countsSoFar.y + 1})) : _Utils_Tuple2(
				function ($) {
					return $.x;
				},
				_Utils_update(
					countsSoFar,
					{x: countsSoFar.x + 1}))) : (valY ? _Utils_Tuple2(
				function ($) {
					return $.u;
				},
				_Utils_update(
					countsSoFar,
					{u: countsSoFar.u + 1})) : _Utils_Tuple2(
				function ($) {
					return $.t;
				},
				_Utils_update(
					countsSoFar,
					{t: countsSoFar.t + 1})));
			var accessor = _v3.a;
			var recurseWith = _v3.b;
			var currentOrd = accessor(countsSoFar);
			var _v4 = A3($author$project$Causality$positionInGroupInner, recurseWith, xRest, yRest);
			var posRest = _v4.a;
			var totalCounts = _v4.b;
			var nData = ((totalCounts.y + totalCounts.x) + totalCounts.u) + totalCounts.t;
			var rectHeight = (valY ? (totalCounts.y + totalCounts.u) : (totalCounts.x + totalCounts.t)) / nData;
			var rectWidth = (valX ? (totalCounts.y + totalCounts.x) : (totalCounts.u + totalCounts.t)) / nData;
			var totalCount = accessor(totalCounts);
			var sqrtAreaPerPoint = $elm$core$Basics$sqrt((rectWidth * rectHeight) / totalCount);
			var minPerRowX = $elm$core$Basics$ceiling(rectWidth / sqrtAreaPerPoint);
			var minPerRowY = $elm$core$Basics$ceiling(rectHeight / sqrtAreaPerPoint);
			var maxFilledY = (_Utils_cmp(totalCount, minPerRowY) > -1) ? minPerRowY : totalCount;
			var stepSize = A2($elm$core$Basics$min, rectWidth / minPerRowX, rectHeight / minPerRowY);
			var remainderX = A2($elm$core$Basics$max, 0, rectWidth - (stepSize * (1 + (((totalCount - 1) / minPerRowY) | 0))));
			var remainderY = A2($elm$core$Basics$max, 0, rectHeight - (stepSize * maxFilledY));
			var xPos = (remainderX * 0.5) + (stepSize * (0.5 + ((currentOrd / minPerRowY) | 0)));
			var yPos = (remainderY * 0.5) + (stepSize * (0.5 + A2($elm$core$Basics$modBy, minPerRowY, currentOrd)));
			return _Utils_Tuple2(
				A2(
					$elm$core$List$cons,
					_Utils_Tuple2(xPos, yPos),
					posRest),
				totalCounts);
		} else {
			return _Utils_Tuple2(_List_Nil, countsSoFar);
		}
	});
var $author$project$Causality$positionInGroup = F2(
	function (xValues, yValues) {
		var _v0 = A3(
			$author$project$Causality$positionInGroupInner,
			{t: 0, u: 0, x: 0, y: 0},
			xValues,
			yValues);
		var res = _v0.a;
		return res;
	});
var $gicentre$elm_vegalite$VegaLite$Rule = 12;
var $gicentre$elm_vegalite$VegaLite$rule = $gicentre$elm_vegalite$VegaLite$mark(12);
var $gicentre$elm_vegalite$VegaLite$toVegaLite = function (spec) {
	return $elm$json$Json$Encode$object(
		A2(
			$elm$core$List$cons,
			_Utils_Tuple2(
				'$schema',
				$elm$json$Json$Encode$string('https://vega.github.io/schema/vega-lite/v5.json')),
			A2(
				$elm$core$List$map,
				function (_v0) {
					var s = _v0.a;
					var v = _v0.b;
					return _Utils_Tuple2(
						$gicentre$elm_vegalite$VegaLite$vlPropertyLabel(s),
						v);
				},
				spec)));
};
var $gicentre$elm_vegalite$VegaLite$VLTransform = 16;
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $gicentre$elm_vegalite$VegaLite$transform = function (transforms) {
	var assemble = function (_v1) {
		var trName = _v1.a;
		var val = _v1.b;
		if (trName === 'multiSpecs') {
			return val;
		} else {
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(trName, val)
					]));
		}
	};
	return $elm$core$List$isEmpty(transforms) ? _Utils_Tuple2(16, $elm$json$Json$Encode$null) : _Utils_Tuple2(
		16,
		A2($elm$json$Json$Encode$list, assemble, transforms));
};
var $elm$core$List$unzip = function (pairs) {
	var step = F2(
		function (_v0, _v1) {
			var x = _v0.a;
			var y = _v0.b;
			var xs = _v1.a;
			var ys = _v1.b;
			return _Utils_Tuple2(
				A2($elm$core$List$cons, x, xs),
				A2($elm$core$List$cons, y, ys));
		});
	return A3(
		$elm$core$List$foldr,
		step,
		_Utils_Tuple2(_List_Nil, _List_Nil),
		pairs);
};
var $author$project$Causality$singlePairWaffleDataSpec = F4(
	function (xValues, yValues, xName, yName) {
		var rule = F2(
			function (coord, pos) {
				return $gicentre$elm_vegalite$VegaLite$asSpec(
					_List_fromArray(
						[
							A2(
							$elm$core$Basics$composeL,
							$gicentre$elm_vegalite$VegaLite$encoding,
							A2(
								$gicentre$elm_vegalite$VegaLite$position,
								coord,
								_List_fromArray(
									[
										$gicentre$elm_vegalite$VegaLite$pDatum(
										$gicentre$elm_vegalite$VegaLite$num(pos))
									])))(_List_Nil),
							$gicentre$elm_vegalite$VegaLite$rule(_List_Nil)
						]));
			});
		var myAxLabel = F2(
			function (val, isX) {
				var _v1 = isX ? _Utils_Tuple2(
					val ? $elm$json$Json$Encode$string('width') : $elm$json$Json$Encode$float(0),
					$elm$json$Json$Encode$string('height')) : _Utils_Tuple2(
					$elm$json$Json$Encode$float(0),
					val ? $elm$json$Json$Encode$float(0) : $elm$json$Json$Encode$string('height'));
				var x = _v1.a;
				var y = _v1.b;
				var _v2 = isX ? _Utils_Tuple3(0, 5, 'top') : _Utils_Tuple3(270, -5, 'bottom');
				var angle = _v2.a;
				var dy = _v2.b;
				var baseline = _v2.c;
				var _v3 = val ? _Utils_Tuple2('True', 'right') : _Utils_Tuple2('False', 'left');
				var text = _v3.a;
				var align = _v3.b;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'mark',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'type',
										$elm$json$Json$Encode$string('text')),
										_Utils_Tuple2(
										'text',
										$elm$json$Json$Encode$string(text)),
										_Utils_Tuple2('x', x),
										_Utils_Tuple2('y', y),
										_Utils_Tuple2(
										'align',
										$elm$json$Json$Encode$string(align)),
										_Utils_Tuple2(
										'baseline',
										$elm$json$Json$Encode$string(baseline)),
										_Utils_Tuple2(
										'dy',
										$elm$json$Json$Encode$float(dy)),
										_Utils_Tuple2(
										'angle',
										$elm$json$Json$Encode$float(angle))
									])))
						]));
			});
		var jitExpr = function (name) {
			return 'if(datum.' + (name + (', (1 - datum.' + (name + ('Mean) + datum.' + (name + ('Pos,  datum.' + (name + 'Pos)')))))));
		};
		var transforms = A2(
			$elm$core$Basics$composeL,
			A2(
				$elm$core$Basics$composeL,
				A2(
					$elm$core$Basics$composeL,
					A2(
						$elm$core$Basics$composeL,
						$gicentre$elm_vegalite$VegaLite$transform,
						A2(
							$gicentre$elm_vegalite$VegaLite$joinAggregate,
							_List_fromArray(
								[
									A3($gicentre$elm_vegalite$VegaLite$opAs, $gicentre$elm_vegalite$VegaLite$opMean, 'x', 'xMean'),
									A3($gicentre$elm_vegalite$VegaLite$opAs, $gicentre$elm_vegalite$VegaLite$opMean, 'y', 'yMean')
								]),
							_List_Nil)),
					A2(
						$gicentre$elm_vegalite$VegaLite$calculateAs,
						jitExpr('x'),
						'xJit')),
				A2(
					$gicentre$elm_vegalite$VegaLite$calculateAs,
					jitExpr('y'),
					'yJit')),
			A2($gicentre$elm_vegalite$VegaLite$calculateAs, 'toString(datum.x) + toString(datum.y)', 'x_y'));
		var internalRule = F2(
			function (coord, name) {
				return $gicentre$elm_vegalite$VegaLite$asSpec(
					_List_fromArray(
						[
							A2(
							$elm$core$Basics$composeL,
							$gicentre$elm_vegalite$VegaLite$transform,
							A2($gicentre$elm_vegalite$VegaLite$calculateAs, '1 - datum.' + name, name + '1m'))(_List_Nil),
							A2(
							$elm$core$Basics$composeL,
							$gicentre$elm_vegalite$VegaLite$encoding,
							A2(
								$gicentre$elm_vegalite$VegaLite$position,
								coord,
								_List_fromArray(
									[
										$gicentre$elm_vegalite$VegaLite$pName(name + '1m'),
										$gicentre$elm_vegalite$VegaLite$pAggregate($gicentre$elm_vegalite$VegaLite$opMean)
									])))(_List_Nil),
							$gicentre$elm_vegalite$VegaLite$rule(_List_Nil)
						]));
			});
		var enc = A2(
			$elm$core$Basics$composeL,
			A2(
				$elm$core$Basics$composeL,
				A2(
					$elm$core$Basics$composeL,
					$gicentre$elm_vegalite$VegaLite$encoding,
					A2(
						$gicentre$elm_vegalite$VegaLite$position,
						0,
						_List_fromArray(
							[
								$gicentre$elm_vegalite$VegaLite$pName('xJit'),
								$gicentre$elm_vegalite$VegaLite$pQuant,
								$gicentre$elm_vegalite$VegaLite$pAxis(
								_List_fromArray(
									[
										$gicentre$elm_vegalite$VegaLite$axTitle(xName)
									]))
							]))),
				A2(
					$gicentre$elm_vegalite$VegaLite$position,
					1,
					_List_fromArray(
						[
							$gicentre$elm_vegalite$VegaLite$pName('yJit'),
							$gicentre$elm_vegalite$VegaLite$pQuant,
							$gicentre$elm_vegalite$VegaLite$pAxis(
							_List_fromArray(
								[
									$gicentre$elm_vegalite$VegaLite$axTitle(yName)
								]))
						]))),
			$gicentre$elm_vegalite$VegaLite$color(
				_List_fromArray(
					[
						$gicentre$elm_vegalite$VegaLite$mName('x_y'),
						$gicentre$elm_vegalite$VegaLite$mNominal,
						$gicentre$elm_vegalite$VegaLite$mLegend(_List_Nil)
					])));
		var spec = $gicentre$elm_vegalite$VegaLite$layer(
			_List_fromArray(
				[
					$gicentre$elm_vegalite$VegaLite$asSpec(
					_List_fromArray(
						[
							transforms(_List_Nil),
							enc(_List_Nil),
							$gicentre$elm_vegalite$VegaLite$circle(_List_Nil)
						])),
					A2(internalRule, 0, 'x'),
					A2(internalRule, 1, 'y'),
					A2(rule, 0, 0),
					A2(rule, 0, 1),
					A2(rule, 1, 0),
					A2(rule, 1, 1),
					A2(myAxLabel, false, false),
					A2(myAxLabel, false, true),
					A2(myAxLabel, true, false),
					A2(myAxLabel, true, true)
				]));
		var config = A2(
			$elm$core$Basics$composeL,
			$gicentre$elm_vegalite$VegaLite$configure,
			$gicentre$elm_vegalite$VegaLite$configuration(
				$gicentre$elm_vegalite$VegaLite$coAxis(
					_List_fromArray(
						[
							$gicentre$elm_vegalite$VegaLite$axcoDomain(false),
							$gicentre$elm_vegalite$VegaLite$axcoTicks(false),
							$gicentre$elm_vegalite$VegaLite$axcoLabels(false),
							$gicentre$elm_vegalite$VegaLite$axcoGrid(false)
						]))));
		var _v0 = $elm$core$List$unzip(
			A2($author$project$Causality$positionInGroup, xValues, yValues));
		var xPos = _v0.a;
		var yPos = _v0.b;
		var data = A2(
			$elm$core$Basics$composeL,
			A2(
				$elm$core$Basics$composeL,
				A2(
					$elm$core$Basics$composeL,
					A2(
						$elm$core$Basics$composeL,
						$gicentre$elm_vegalite$VegaLite$dataFromColumns(_List_Nil),
						A2(
							$gicentre$elm_vegalite$VegaLite$dataColumn,
							'x',
							$gicentre$elm_vegalite$VegaLite$boos(xValues))),
					A2(
						$gicentre$elm_vegalite$VegaLite$dataColumn,
						'y',
						$gicentre$elm_vegalite$VegaLite$boos(yValues))),
				A2(
					$gicentre$elm_vegalite$VegaLite$dataColumn,
					'xPos',
					$gicentre$elm_vegalite$VegaLite$nums(xPos))),
			A2(
				$gicentre$elm_vegalite$VegaLite$dataColumn,
				'yPos',
				$gicentre$elm_vegalite$VegaLite$nums(yPos)))(_List_Nil);
		return $gicentre$elm_vegalite$VegaLite$toVegaLite(
			_List_fromArray(
				[
					data,
					config(_List_Nil),
					spec
				]));
	});
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlJson(value));
	});
var $elm$html$Html$Attributes$property = $elm$virtual_dom$VirtualDom$property;
var $author$project$View$vegaPlot = function (spec) {
	return A3(
		$elm$html$Html$node,
		'vega-plot',
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$property, 'spec', spec)
			]),
		_List_Nil);
};
var $author$project$Causality$viewOutcomeSubplot = F6(
	function (xOrd, yOrd, xValues, yValues, xName, yName) {
		return (_Utils_cmp(xOrd, yOrd) < 0) ? A2(
			$elm$html$Html$td,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('waffle')
				]),
			_List_fromArray(
				[
					$author$project$View$vegaPlot(
					A4($author$project$Causality$singlePairWaffleDataSpec, xValues, yValues, xName, yName))
				])) : A2($elm$html$Html$td, _List_Nil, _List_Nil);
	});
var $author$project$Causality$viewOutcome = F2(
	function (sorted, outcome) {
		var varNames = $author$project$Causality$variableNames(sorted);
		var plotRow = F2(
			function (yOrd, _v1) {
				var yName = _v1.a;
				var yVals = _v1.b;
				return A2(
					$elm$html$Html$tr,
					_List_Nil,
					A2(
						$elm$core$List$indexedMap,
						F2(
							function (ord, _v2) {
								var name = _v2.a;
								var vals = _v2.b;
								return A6($author$project$Causality$viewOutcomeSubplot, ord, yOrd, vals, yVals, name, yName);
							}),
						A3($elm$core$List$map2, $elm$core$Tuple$pair, varNames, outcome)));
			});
		var allRows = function () {
			if (!varNames.b) {
				return _List_Nil;
			} else {
				if (!varNames.b.b) {
					return _List_Nil;
				} else {
					return A2(
						$elm$core$List$indexedMap,
						plotRow,
						A3($elm$core$List$map2, $elm$core$Tuple$pair, varNames, outcome));
				}
			}
		}();
		return A2($elm$html$Html$table, _List_Nil, allRows);
	});
var $author$project$Association$viewExperiment = F3(
	function (spec, id, _v0) {
		var experiment = _v0.a;
		var data = _v0.b;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('experiment')
				]),
			_List_fromArray(
				[
					$author$project$View$experimentTitle(id),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'N = ' + ($elm$core$String$fromInt(experiment) + (', CZK ' + $elm$core$String$fromInt(
								$author$project$Association$costExperiment(experiment)))))
						])),
					A3($elm$html$Html$Lazy$lazy2, $author$project$Causality$viewOutcome, spec.S, data)
				]));
	});
var $author$project$Association$viewGuess = F2(
	function (spec, guess) {
		var _v0 = $author$project$Association$specToNames(spec);
		var name0 = _v0.a;
		var name1 = _v0.b;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A3($author$project$Causality$causalityDescription, name0, name1, guess)
				]));
	});
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $author$project$Association$viewHeader = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('scenarioHeader')
		]),
	_List_fromArray(
		[
			A2(
			$elm$html$Html$h2,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Is there an association?')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('')
				]))
		]));
var $author$project$Association$SetN = $elm$core$Basics$identity;
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$View$nChooser = F2(
	function (setNMsg, currentN) {
		return A2(
			$elm$html$Html$input,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$type_('text'),
					$elm$html$Html$Attributes$value(
					$elm$core$String$fromInt(currentN)),
					$elm$html$Html$Events$onInput(setNMsg)
				]),
			_List_Nil);
	});
var $author$project$Association$viewProposedExperiment = F2(
	function (_v0, experiment) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Run an observational study with '),
					A2($author$project$View$nChooser, $elm$core$Basics$identity, experiment),
					$elm$html$Html$text(' participants.')
				]));
	});
var $author$project$Association$SetGuess = $elm$core$Basics$identity;
var $author$project$Causality$LeftNeg = 4;
var $author$project$Causality$LeftPos = 3;
var $author$project$Causality$causalityDirectionFromShortString = function (cat) {
	switch (cat) {
		case 'NoCause':
			return 0;
		case 'RightPos':
			return 1;
		case 'RightNeg':
			return 2;
		case 'LeftPos':
			return 3;
		case 'LeftNeg':
			return 4;
		default:
			return 0;
	}
};
var $author$project$Causality$causalityDirectionToShortString = function (cat) {
	switch (cat) {
		case 0:
			return 'NoCause';
		case 1:
			return 'RightPos';
		case 2:
			return 'RightNeg';
		case 3:
			return 'LeftPos';
		default:
			return 'LeftNeg';
	}
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $author$project$View$onChange = function (messageCreator) {
	return A2(
		$elm$html$Html$Events$on,
		'change',
		A2($elm$json$Json$Decode$map, messageCreator, $elm$html$Html$Events$targetValue));
};
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$select = _VirtualDom_node('select');
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $author$project$Association$viewProposedGuess = F2(
	function (spec, guess) {
		var singleOption = function (val) {
			return A2(
				$elm$html$Html$option,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$selected(
						_Utils_eq(guess, val)),
						$elm$html$Html$Attributes$value(
						$author$project$Causality$causalityDirectionToShortString(val))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						$author$project$Causality$causalityDirectionToString(val))
					]));
		};
		var _v0 = $author$project$Association$specToNames(spec);
		var name0 = _v0.a;
		var name1 = _v0.b;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('I believe '),
					A2(
					$elm$html$Html$em,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(name0)
						])),
					$elm$html$Html$text(' '),
					A2(
					$elm$html$Html$select,
					_List_fromArray(
						[
							$author$project$View$onChange(
							A2($elm$core$Basics$composeR, $author$project$Causality$causalityDirectionFromShortString, $elm$core$Basics$identity))
						]),
					_List_fromArray(
						[
							singleOption(0),
							singleOption(1),
							singleOption(2)
						])),
					$elm$html$Html$text(' '),
					A2(
					$elm$html$Html$em,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(name1)
						])),
					$elm$html$Html$text('.')
				]));
	});
var $author$project$Association$viewAdapter = {bv: $author$project$Association$viewCostCommentary, bw: $author$project$Association$viewExperiment, bx: $author$project$Association$viewGuess, by: $author$project$Association$viewHeader, bz: $author$project$Association$viewProposedExperiment, bA: $author$project$Association$viewProposedGuess};
var $author$project$Association$adapter = {ba: $author$project$Association$initAdapter, bd: $author$project$Association$logicAdapter, bu: $author$project$Association$viewAdapter};
var $author$project$SingleRelationship$initAdapter = {
	a$: {B: 0, K: 100, F: false},
	a0: 0,
	bb: 4
};
var $author$project$Causality$costPerExperiment = function (randomized) {
	return randomized ? 50000 : 2000;
};
var $author$project$Causality$costPerParticipant = function (randomized) {
	return randomized ? 2000 : 100;
};
var $author$project$Causality$costExperiment = function (exp) {
	return (exp.K * $author$project$Causality$costPerParticipant(exp.F)) + $author$project$Causality$costPerExperiment(exp.F);
};
var $author$project$Causality$generatorRandomized = F3(
	function (dag, intervention, nSubj) {
		return A2(
			$elm$random$Random$map,
			A2($author$project$Causality$dataFromNoise, dag, intervention),
			A2(
				$author$project$Causality$probitNoiseGenerator,
				$elm$core$List$length(dag.aR),
				nSubj));
	});
var $author$project$Causality$outcomeGenerator = F2(
	function (sorted, experiment) {
		return experiment.F ? A3($author$project$Causality$generatorRandomized, sorted, experiment.B, experiment.K) : A2($author$project$Causality$generatorObservational, sorted, experiment.K);
	});
var $author$project$SingleRelationship$generator = F2(
	function (spec, experiment) {
		return A2($author$project$Causality$outcomeGenerator, spec.S, experiment);
	});
var $author$project$SingleRelationship$specToNames = function (spec) {
	var _v0 = spec.S.aR;
	if (_v0.b && _v0.b.b) {
		var a = _v0.a;
		var _v1 = _v0.b;
		var b = _v1.a;
		return _Utils_Tuple2(a.aC, b.aC);
	} else {
		return _Utils_Tuple2('Error', 'Error');
	}
};
var $author$project$SingleRelationship$guessEval = F2(
	function (spec, guess) {
		var _v0 = $author$project$SingleRelationship$specToNames(spec);
		var name0 = _v0.a;
		var name1 = _v0.b;
		return _Utils_eq(guess, spec.ac) ? _Utils_Tuple2(
			true,
			$elm$html$Html$text('')) : _Utils_Tuple2(
			false,
			A3($author$project$Causality$causalityDescription, name0, name1, spec.ac));
	});
var $author$project$Causality$categoryGenerator = A2(
	$elm$random$Random$uniform,
	0,
	_List_fromArray(
		[2, 1, 4, 3]));
var $author$project$SingleRelationship$specGenerator = function () {
	var varNames = _List_fromArray(
		['A', 'B']);
	var variables = A2(
		$elm$random$Random$map,
		function (intercepts) {
			return A3($elm$core$List$map2, $author$project$Causality$Variable, varNames, intercepts);
		},
		A2($elm$random$Random$list, 2, $author$project$Causality$interceptGenerator));
	var edgeListFromAssoc = F2(
		function (assocVal, contribVal) {
			switch (assocVal) {
				case 0:
					return _List_Nil;
				case 1:
					return _List_fromArray(
						[
							{a4: 0, bc: contribVal, bp: 1}
						]);
				case 2:
					return _List_fromArray(
						[
							{a4: 0, bc: contribVal, bp: 1}
						]);
				case 3:
					return _List_fromArray(
						[
							{a4: 1, bc: contribVal, bp: 0}
						]);
				default:
					return _List_fromArray(
						[
							{a4: 1, bc: contribVal, bp: 0}
						]);
			}
		});
	var graphFromCauses = F2(
		function (assocVal, contribVal) {
			return A2(
				$elm_community$graph$Graph$fromNodesAndEdges,
				A2(
					$elm$core$List$map,
					function (x) {
						return A2($elm_community$graph$Graph$Node, x, x);
					},
					_List_fromArray(
						[0, 1])),
				A2(edgeListFromAssoc, assocVal, contribVal));
		});
	var sortedFromCausesAndVars = F3(
		function (assocVal, contribVal, vars) {
			return {
				S: function () {
					var _v1 = $elm_community$graph$Graph$checkAcyclic(
						A2(graphFromCauses, assocVal, contribVal));
					if (!_v1.$) {
						var dag = _v1.a;
						return $elm_community$graph$Graph$topologicalSort(dag);
					} else {
						return _List_Nil;
					}
				}(),
				aR: vars
			};
		});
	var specFromData = F2(
		function (_v0, vars) {
			var assocVal = _v0.a;
			var contribVal = _v0.b;
			return {
				ac: assocVal,
				aq: contribVal,
				S: A3(sortedFromCausesAndVars, assocVal, contribVal, vars)
			};
		});
	var causeContrib = A2(
		$elm$random$Random$andThen,
		function (assocVal) {
			return A2(
				$elm$random$Random$map,
				$elm$core$Tuple$pair(assocVal),
				$author$project$Causality$contribGenerator(assocVal));
		},
		$author$project$Causality$categoryGenerator);
	return A3($elm$random$Random$map2, specFromData, causeContrib, variables);
}();
var $author$project$Causality$updateExperiment = F2(
	function (msg, experiment) {
		switch (msg.$) {
			case 0:
				var newN = msg.a;
				var _v1 = $elm$core$String$toInt(newN);
				if (!_v1.$) {
					var n = _v1.a;
					return _Utils_update(
						experiment,
						{
							K: A2($elm$core$Basics$min, n, $author$project$Causality$maxN)
						});
				} else {
					return experiment;
				}
			case 1:
				var newRand = msg.a;
				var newIntervention = _Utils_eq(experiment.B, $author$project$Causality$noIntervention) ? 1 : experiment.B;
				return _Utils_update(
					experiment,
					{B: newIntervention, F: newRand});
			default:
				var newIntervention = msg.a;
				return _Utils_update(
					experiment,
					{B: newIntervention});
		}
	});
var $author$project$SingleRelationship$logicAdapter = {a_: $author$project$Causality$costExperiment, a5: $author$project$SingleRelationship$generator, a6: $author$project$SingleRelationship$guessEval, bm: $author$project$SingleRelationship$specGenerator, bs: $author$project$Causality$updateExperiment, bt: $author$project$Association$updateGuess};
var $elm$html$Html$br = _VirtualDom_node('br');
var $author$project$Causality$viewCostCommentary = A2(
	$elm$html$Html$p,
	_List_Nil,
	_List_fromArray(
		[
			$elm$html$Html$text(
			'Observational study costs CZK ' + ($elm$core$String$fromInt(
				$author$project$Causality$costPerExperiment(false)) + (' + CZK ' + ($elm$core$String$fromInt(
				$author$project$Causality$costPerParticipant(false)) + ' per participant')))),
			A2($elm$html$Html$br, _List_Nil, _List_Nil),
			$elm$html$Html$text(
			'Randomized study costs CZK ' + ($elm$core$String$fromInt(
				$author$project$Causality$costPerExperiment(true)) + (' + CZK ' + ($elm$core$String$fromInt(
				$author$project$Causality$costPerParticipant(true)) + ' per participant'))))
		]));
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $author$project$Causality$viewExperiment = F3(
	function (sorted, id, _v0) {
		var experiment = _v0.a;
		var data = _v0.b;
		var typeText = experiment.F ? ('Randomizing ' + function () {
			var _v1 = A2(
				$elm$core$List$filter,
				function (_v2) {
					var varId = _v2.a;
					return _Utils_eq(varId, experiment.B);
				},
				A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, sorted.aR));
			if (!_v1.b) {
				return 'Error';
			} else {
				var _v3 = _v1.a;
				var _var = _v3.b;
				return _var.aC;
			}
		}()) : 'Observational study';
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('experiment')
				]),
			_List_fromArray(
				[
					$author$project$View$experimentTitle(id),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$strong,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(typeText)
								])),
							A2($elm$html$Html$br, _List_Nil, _List_Nil),
							$elm$html$Html$text(
							'N = ' + ($elm$core$String$fromInt(experiment.K) + (', CZK ' + $elm$core$String$fromInt(
								$author$project$Causality$costExperiment(experiment)))))
						])),
					A3($elm$html$Html$Lazy$lazy2, $author$project$Causality$viewOutcome, sorted, data)
				]));
	});
var $author$project$SingleRelationship$viewExperiment = function (spec) {
	return $author$project$Causality$viewExperiment(spec.S);
};
var $author$project$SingleRelationship$viewGuess = F2(
	function (spec, guess) {
		var _v0 = $author$project$SingleRelationship$specToNames(spec);
		var name0 = _v0.a;
		var name1 = _v0.b;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A3($author$project$Causality$causalityDescription, name0, name1, guess)
				]));
	});
var $author$project$SingleRelationship$viewHeader = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('scenarioHeader')
		]),
	_List_fromArray(
		[
			A2(
			$elm$html$Html$h2,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Single causal relationship')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('')
				]))
		]));
var $author$project$Causality$SetIntervention = function (a) {
	return {$: 2, a: a};
};
var $author$project$Causality$SetN = function (a) {
	return {$: 0, a: a};
};
var $author$project$Causality$SetRandomized = function (a) {
	return {$: 1, a: a};
};
var $author$project$Utils$boolFromString = function (s) {
	return s === 'True';
};
var $author$project$Utils$boolToString = function (b) {
	return b ? 'True' : 'False';
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Causality$viewProposedExperiment = F2(
	function (sorted, experiment) {
		var randomizedOption = F2(
			function (val, label) {
				return A2(
					$elm$html$Html$option,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$selected(
							_Utils_eq(experiment.F, val)),
							$elm$html$Html$Attributes$value(
							$author$project$Utils$boolToString(val))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						]));
			});
		var interventionOption = F2(
			function (id, _var) {
				return A2(
					$elm$html$Html$option,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$selected(
							_Utils_eq(experiment.B, id)),
							$elm$html$Html$Attributes$value(
							$elm$core$String$fromInt(id))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(_var.aC)
						]));
			});
		var intervention = experiment.F ? A2(
			$elm$html$Html$span,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(', randomizing '),
					A2(
					$elm$html$Html$select,
					_List_fromArray(
						[
							$author$project$View$onChange(
							A2(
								$elm$core$Basics$composeR,
								$elm$core$String$toInt,
								A2(
									$elm$core$Basics$composeR,
									$elm$core$Maybe$withDefault($author$project$Causality$noIntervention),
									$author$project$Causality$SetIntervention)))
						]),
					A2($elm$core$List$indexedMap, interventionOption, sorted.aR))
				])) : $elm$html$Html$text('');
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Run an '),
					A2(
					$elm$html$Html$select,
					_List_fromArray(
						[
							$author$project$View$onChange(
							A2($elm$core$Basics$composeR, $author$project$Utils$boolFromString, $author$project$Causality$SetRandomized))
						]),
					_List_fromArray(
						[
							A2(randomizedOption, false, 'observational'),
							A2(randomizedOption, true, 'randomized')
						])),
					$elm$html$Html$text(' study '),
					intervention,
					$elm$html$Html$text(' with '),
					A2($author$project$View$nChooser, $author$project$Causality$SetN, experiment.K),
					$elm$html$Html$text(' participants.')
				]));
	});
var $author$project$SingleRelationship$viewProposedExperiment = function (spec) {
	return $author$project$Causality$viewProposedExperiment(spec.S);
};
var $author$project$Causality$causalityChooser = F2(
	function (causeMsg, currentVal) {
		var singleOption = function (val) {
			return A2(
				$elm$html$Html$option,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$selected(
						_Utils_eq(currentVal, val)),
						$elm$html$Html$Attributes$value(
						$author$project$Causality$causalityDirectionToShortString(val))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						$author$project$Causality$causalityDirectionToString(val))
					]));
		};
		return A2(
			$elm$html$Html$select,
			_List_fromArray(
				[
					$author$project$View$onChange(
					A2($elm$core$Basics$composeR, $author$project$Causality$causalityDirectionFromShortString, causeMsg))
				]),
			_List_fromArray(
				[
					singleOption(0),
					singleOption(1),
					singleOption(2),
					singleOption(3),
					singleOption(4)
				]));
	});
var $author$project$Causality$causalityProposedGuess = F4(
	function (name1, name2, causeMsg, currentVal) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$em,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(name1)
						])),
					$elm$html$Html$text(' '),
					A2($author$project$Causality$causalityChooser, causeMsg, currentVal),
					$elm$html$Html$text(' '),
					A2(
					$elm$html$Html$em,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(name2)
						]))
				]));
	});
var $author$project$SingleRelationship$viewProposedGuess = F2(
	function (spec, guess) {
		var _v0 = $author$project$SingleRelationship$specToNames(spec);
		var name0 = _v0.a;
		var name1 = _v0.b;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('I believe '),
					A4($author$project$Causality$causalityProposedGuess, name0, name1, $elm$core$Basics$identity, guess)
				]));
	});
var $author$project$SingleRelationship$viewAdapter = {bv: $author$project$Causality$viewCostCommentary, bw: $author$project$SingleRelationship$viewExperiment, bx: $author$project$SingleRelationship$viewGuess, by: $author$project$SingleRelationship$viewHeader, bz: $author$project$SingleRelationship$viewProposedExperiment, bA: $author$project$SingleRelationship$viewProposedGuess};
var $author$project$SingleRelationship$adapter = {ba: $author$project$SingleRelationship$initAdapter, bd: $author$project$SingleRelationship$logicAdapter, bu: $author$project$SingleRelationship$viewAdapter};
var $author$project$TwoRelationships$generator = F2(
	function (spec, experiment) {
		return A2($author$project$Causality$outcomeGenerator, spec.S, experiment);
	});
var $author$project$TwoRelationships$specToNames = function (spec) {
	var _v0 = spec.S.aR;
	if ((_v0.b && _v0.b.b) && _v0.b.b.b) {
		var a = _v0.a;
		var _v1 = _v0.b;
		var b = _v1.a;
		var _v2 = _v1.b;
		var c = _v2.a;
		return _Utils_Tuple3(a.aC, b.aC, c.aC);
	} else {
		return _Utils_Tuple3('Error', 'Error', 'Error');
	}
};
var $author$project$TwoRelationships$guessEval = F2(
	function (spec, guess) {
		var _v0 = $author$project$TwoRelationships$specToNames(spec);
		var name0 = _v0.a;
		var name1 = _v0.b;
		var name2 = _v0.c;
		return (_Utils_eq(guess.p, spec.p) && _Utils_eq(guess.q, spec.q)) ? _Utils_Tuple2(
			true,
			$elm$html$Html$text('')) : _Utils_Tuple2(
			false,
			A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A3($author$project$Causality$causalityDescription, name0, name1, spec.p),
						A3($author$project$Causality$causalityDescription, name1, name2, spec.q)
					])));
	});
var $author$project$TwoRelationships$specGenerator = function () {
	var varNames = _List_fromArray(
		['A', 'B', 'C']);
	var variables = A2(
		$elm$random$Random$map,
		function (intercepts) {
			return A3($elm$core$List$map2, $author$project$Causality$Variable, varNames, intercepts);
		},
		A2($elm$random$Random$list, 3, $author$project$Causality$interceptGenerator));
	var edgeListFromCause = F4(
		function (id1, id2, cause, contrib) {
			switch (cause) {
				case 0:
					return _List_Nil;
				case 1:
					return _List_fromArray(
						[
							{a4: id1, bc: contrib, bp: id2}
						]);
				case 2:
					return _List_fromArray(
						[
							{a4: id1, bc: contrib, bp: id2}
						]);
				case 3:
					return _List_fromArray(
						[
							{a4: id2, bc: contrib, bp: id1}
						]);
				default:
					return _List_fromArray(
						[
							{a4: id2, bc: contrib, bp: id1}
						]);
			}
		});
	var graphFromCauses = F2(
		function (_v4, _v5) {
			var cause01 = _v4.a;
			var cause12 = _v4.b;
			var contrib01 = _v5.a;
			var contrib12 = _v5.b;
			return A2(
				$elm_community$graph$Graph$fromNodesAndEdges,
				A2(
					$elm$core$List$map,
					function (x) {
						return A2($elm_community$graph$Graph$Node, x, x);
					},
					_List_fromArray(
						[0, 1, 2])),
				_Utils_ap(
					A4(edgeListFromCause, 0, 1, cause01, contrib01),
					A4(edgeListFromCause, 1, 2, cause12, contrib12)));
		});
	var sortedFromCausesAndVars = F3(
		function (causesVals, contribVals, vars) {
			return {
				S: function () {
					var _v3 = $elm_community$graph$Graph$checkAcyclic(
						A2(graphFromCauses, causesVals, contribVals));
					if (!_v3.$) {
						var dag = _v3.a;
						return $elm_community$graph$Graph$topologicalSort(dag);
					} else {
						return _List_Nil;
					}
				}(),
				aR: vars
			};
		});
	var specFromData = F2(
		function (_v1, vars) {
			var _v2 = _v1.a;
			var cause01 = _v2.a;
			var cause12 = _v2.b;
			var contribVals = _v1.b;
			return {
				p: cause01,
				q: cause12,
				S: A3(
					sortedFromCausesAndVars,
					_Utils_Tuple2(cause01, cause12),
					contribVals,
					vars)
			};
		});
	var causesContribs = A2(
		$elm$random$Random$andThen,
		function (_v0) {
			var c1 = _v0.a;
			var c2 = _v0.b;
			return A2(
				$elm$random$Random$map,
				$elm$core$Tuple$pair(
					_Utils_Tuple2(c1, c2)),
				A3(
					$elm$random$Random$map2,
					$elm$core$Tuple$pair,
					$author$project$Causality$contribGenerator(c1),
					$author$project$Causality$contribGenerator(c2)));
		},
		A3($elm$random$Random$map2, $elm$core$Tuple$pair, $author$project$Causality$categoryGenerator, $author$project$Causality$categoryGenerator));
	return A3($elm$random$Random$map2, specFromData, causesContribs, variables);
}();
var $author$project$TwoRelationships$updateGuess = F2(
	function (msg, old) {
		if (!msg.$) {
			var g = msg.a;
			return _Utils_update(
				old,
				{p: g});
		} else {
			var g = msg.a;
			return _Utils_update(
				old,
				{q: g});
		}
	});
var $author$project$TwoRelationships$viewExperiment = function (spec) {
	return $author$project$Causality$viewExperiment(spec.S);
};
var $author$project$TwoRelationships$viewGuess = F2(
	function (spec, guess) {
		var _v0 = $author$project$TwoRelationships$specToNames(spec);
		var name0 = _v0.a;
		var name1 = _v0.b;
		var name2 = _v0.c;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A3($author$project$Causality$causalityDescription, name0, name1, guess.p),
					A3($author$project$Causality$causalityDescription, name1, name2, guess.q)
				]));
	});
var $author$project$TwoRelationships$viewHeader = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('scenarioHeader')
		]),
	_List_fromArray(
		[
			A2(
			$elm$html$Html$h2,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Two way causality')
				]))
		]));
var $author$project$TwoRelationships$viewProposedExperiment = function (spec) {
	return $author$project$Causality$viewProposedExperiment(spec.S);
};
var $author$project$TwoRelationships$SetCause01 = function (a) {
	return {$: 0, a: a};
};
var $author$project$TwoRelationships$SetCause12 = function (a) {
	return {$: 1, a: a};
};
var $author$project$TwoRelationships$viewProposedGuess = F2(
	function (spec, guess) {
		var _v0 = $author$project$TwoRelationships$specToNames(spec);
		var name0 = _v0.a;
		var name1 = _v0.b;
		var name2 = _v0.c;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('I believe '),
					A4($author$project$Causality$causalityProposedGuess, name0, name1, $author$project$TwoRelationships$SetCause01, guess.p),
					$elm$html$Html$text(' AND '),
					A4($author$project$Causality$causalityProposedGuess, name1, name2, $author$project$TwoRelationships$SetCause12, guess.q)
				]));
	});
var $author$project$TwoRelationships$adapter = {
	ba: {
		a$: {B: 0, K: 100, F: false},
		a0: {p: 0, q: 0},
		bb: 3
	},
	bd: {a_: $author$project$Causality$costExperiment, a5: $author$project$TwoRelationships$generator, a6: $author$project$TwoRelationships$guessEval, bm: $author$project$TwoRelationships$specGenerator, bs: $author$project$Causality$updateExperiment, bt: $author$project$TwoRelationships$updateGuess},
	bu: {bv: $author$project$Causality$viewCostCommentary, bw: $author$project$TwoRelationships$viewExperiment, bx: $author$project$TwoRelationships$viewGuess, by: $author$project$TwoRelationships$viewHeader, bz: $author$project$TwoRelationships$viewProposedExperiment, bA: $author$project$TwoRelationships$viewProposedGuess}
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $author$project$Game$init = function (adapter) {
	return {an: $elm$core$Maybe$Nothing, k: _List_Nil, E: adapter.ba.a$, N: adapter.ba.a0};
};
var $author$project$Game$SpecGenerated = function (a) {
	return {$: 0, a: a};
};
var $elm$random$Random$Generate = $elm$core$Basics$identity;
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0;
		return A2($elm$random$Random$map, func, generator);
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			A2($elm$random$Random$map, tagger, generator));
	});
var $author$project$Game$initCmd = function (adapter) {
	return A2($elm$random$Random$generate, $author$project$Game$SpecGenerated, adapter.bd.bm);
};
var $elm$core$Platform$Cmd$map = _Platform_map;
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple2(
		{
			ab: $author$project$Game$init($author$project$Association$adapter),
			L: 0,
			W: $author$project$Game$init($author$project$SingleRelationship$adapter),
			Z: $author$project$Game$init($author$project$TwoRelationships$adapter)
		},
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					A2(
					$elm$core$Platform$Cmd$map,
					$author$project$Main$AssocMsg,
					$author$project$Game$initCmd($author$project$Association$adapter)),
					A2(
					$elm$core$Platform$Cmd$map,
					$author$project$Main$SingleRel,
					$author$project$Game$initCmd($author$project$SingleRelationship$adapter)),
					A2(
					$elm$core$Platform$Cmd$map,
					$author$project$Main$TwoRel,
					$author$project$Game$initCmd($author$project$TwoRelationships$adapter))
				])));
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Game$DataGenerated = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Game$allowMoreExperiments = function (instance) {
	return $elm$core$List$length(instance.H) < 100;
};
var $author$project$Game$update = F3(
	function (adapter, msg, scenario) {
		switch (msg.$) {
			case 0:
				var sp = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						scenario,
						{
							k: A2(
								$elm$core$List$cons,
								{H: _List_Nil, J: $elm$core$Maybe$Nothing, w: sp},
								scenario.k)
						}),
					$elm$core$Platform$Cmd$none);
			case 1:
				var _v1 = scenario.k;
				if (_v1.b) {
					var head = _v1.a;
					return _Utils_Tuple2(
						scenario,
						A2(
							$elm$random$Random$generate,
							$author$project$Game$DataGenerated(scenario.E),
							A2(adapter.bd.a5, head.w, scenario.E)));
				} else {
					return _Utils_Tuple2(scenario, $elm$core$Platform$Cmd$none);
				}
			case 2:
				var exp = msg.a;
				var data = msg.b;
				var _v2 = scenario.k;
				if (_v2.b) {
					var active = _v2.a;
					var rest = _v2.b;
					return $author$project$Game$allowMoreExperiments(active) ? _Utils_Tuple2(
						_Utils_update(
							scenario,
							{
								k: A2(
									$elm$core$List$cons,
									_Utils_update(
										active,
										{
											H: A2(
												$elm$core$List$cons,
												_Utils_Tuple2(exp, data),
												active.H)
										}),
									rest)
							}),
						$elm$core$Platform$Cmd$none) : _Utils_Tuple2(scenario, $elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(scenario, $elm$core$Platform$Cmd$none);
				}
			case 3:
				var eMsg = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						scenario,
						{
							E: A2(adapter.bd.bs, eMsg, scenario.E)
						}),
					$elm$core$Platform$Cmd$none);
			case 4:
				var gMsg = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						scenario,
						{
							N: A2(adapter.bd.bt, gMsg, scenario.N)
						}),
					$elm$core$Platform$Cmd$none);
			case 5:
				var _v3 = scenario.k;
				if (_v3.b) {
					var active = _v3.a;
					var rest = _v3.b;
					return _Utils_Tuple2(
						_Utils_update(
							scenario,
							{
								k: A2(
									$elm$core$List$cons,
									_Utils_update(
										active,
										{
											J: $elm$core$Maybe$Just(scenario.N)
										}),
									rest),
								N: adapter.ba.a0
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(scenario, $elm$core$Platform$Cmd$none);
				}
			default:
				return _Utils_Tuple2(
					scenario,
					A2($elm$random$Random$generate, $author$project$Game$SpecGenerated, adapter.bd.bm));
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var assocMsg = msg.a;
				var _v1 = A3($author$project$Game$update, $author$project$Association$adapter, assocMsg, model.ab);
				var newAssocModel = _v1.a;
				var assocCmd = _v1.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ab: newAssocModel}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$AssocMsg, assocCmd));
			case 1:
				var singleMsg = msg.a;
				var _v2 = A3($author$project$Game$update, $author$project$SingleRelationship$adapter, singleMsg, model.W);
				var newSingleRelModel = _v2.a;
				var singleCmd = _v2.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{W: newSingleRelModel}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$SingleRel, singleCmd));
			case 2:
				var twoWayMsg = msg.a;
				var _v3 = A3($author$project$Game$update, $author$project$TwoRelationships$adapter, twoWayMsg, model.Z);
				var newTwoWayModel = _v3.a;
				var twoWayCmd = _v3.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{Z: newTwoWayModel}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$TwoRel, twoWayCmd));
			default:
				var page = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{L: page}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$SingleRelPage = 1;
var $author$project$Main$TwoRelPage = 2;
var $author$project$Main$ifActive = F2(
	function (_v0, _v1) {
		var activePage = _v0.a;
		var page = _v0.b;
		var activeOutput = _v1.a;
		var inactiveOutput = _v1.b;
		return _Utils_eq(page, activePage) ? activeOutput : inactiveOutput;
	});
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Game$ExperimentChanged = function (a) {
	return {$: 3, a: a};
};
var $author$project$Game$GuessChanged = function (a) {
	return {$: 4, a: a};
};
var $author$project$Game$MakeGuess = {$: 5};
var $author$project$Game$RunExperiment = {$: 1};
var $author$project$Game$computeCost = F2(
	function (adapter, instance) {
		return $elm$core$List$sum(
			A2(
				$elm$core$List$map,
				adapter.a_,
				A2($elm$core$List$map, $elm$core$Tuple$first, instance.H)));
	});
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$Game$NewInstance = {$: 6};
var $author$project$Game$viewScenarioControl = F2(
	function (adapter, scenario) {
		var allowNewInstance = function () {
			var _v0 = scenario.k;
			if (_v0.b) {
				var instance = _v0.a;
				var _v1 = instance.J;
				if (!_v1.$) {
					return true;
				} else {
					return false;
				}
			} else {
				return true;
			}
		}();
		return allowNewInstance ? A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Events$onClick($author$project$Game$NewInstance),
							$elm$html$Html$Attributes$value('Start new instance')
						]),
					_List_Nil)
				])) : $elm$html$Html$text('');
	});
var $author$project$Game$viewGameControls = F2(
	function (adapter, scenario) {
		var _v0 = scenario.k;
		if (_v0.b) {
			var instance = _v0.a;
			var _v1 = function () {
				var _v2 = instance.J;
				if (!_v2.$) {
					return _Utils_Tuple2(
						true,
						$elm$html$Html$text(''));
				} else {
					return _Utils_Tuple2(
						false,
						A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('proposedGuess')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$h3,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('Ready to make a guess?')
										])),
									A2(
									$elm$html$Html$map,
									$author$project$Game$GuessChanged,
									A2(adapter.bu.bA, instance.w, scenario.N)),
									A2(
									$elm$html$Html$input,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Attributes$class('guessButton'),
											$elm$html$Html$Events$onClick($author$project$Game$MakeGuess),
											$elm$html$Html$Attributes$value('Make a guess!')
										]),
									_List_Nil)
								])));
				}
			}();
			var wasGuessed = _v1.a;
			var guessElement = _v1.b;
			var activeElement = (!wasGuessed) ? A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h3,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Run an experiment')
							])),
						A2(
						$elm$html$Html$map,
						$author$project$Game$ExperimentChanged,
						A2(adapter.bu.bz, instance.w, scenario.E)),
						$author$project$Game$allowMoreExperiments(instance) ? A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2($elm$html$Html$map, $elm$core$Basics$never, adapter.bu.bv),
								A2($elm$html$Html$br, _List_Nil, _List_Nil),
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('button'),
										$elm$html$Html$Events$onClick($author$project$Game$RunExperiment),
										$elm$html$Html$Attributes$value(
										'Gather more data for CZK ' + $elm$core$String$fromInt(
											adapter.bd.a_(scenario.E)))
									]),
								_List_Nil)
							])) : $elm$html$Html$text('Reached the maximum number of experiments'),
						A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$strong,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Total cost so far: ')
									])),
								$elm$html$Html$text(
								'CZK ' + $elm$core$String$fromInt(
									A2($author$project$Game$computeCost, adapter.bd, instance)))
							]))
					])) : A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$strong,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Total cost: ')
							])),
						$elm$html$Html$text(
						'CZK ' + $elm$core$String$fromInt(
							A2($author$project$Game$computeCost, adapter.bd, instance)))
					]));
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('controls')
					]),
				_List_fromArray(
					[
						A2($elm$html$Html$map, $elm$core$Basics$never, adapter.bu.by),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('scenarioControl')
							]),
						_List_fromArray(
							[
								A2($author$project$Game$viewScenarioControl, adapter, scenario)
							])),
						activeElement,
						guessElement
					]));
		} else {
			return $elm$html$Html$text('');
		}
	});
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $author$project$Game$reverseIndexedMap = F2(
	function (f, l) {
		var ids = $elm$core$List$reverse(
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(l) - 1));
		return A3($elm$core$List$map2, f, ids, l);
	});
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $author$project$Game$withReverseIds = function (items) {
	var ids = A2(
		$elm$core$List$map,
		$elm$core$String$fromInt,
		$elm$core$List$reverse(
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(items) - 1)));
	return A3($elm$core$List$map2, $elm$core$Tuple$pair, ids, items);
};
var $author$project$Game$viewSingleHistory = F4(
	function (adapter, active, id, instance) {
		var experiments = A2(
			$elm$core$List$map,
			$elm$html$Html$map($elm$core$Basics$never),
			A2(
				$author$project$Game$reverseIndexedMap,
				adapter.bu.bw(instance.w),
				instance.H));
		var dataDesc = function () {
			if (experiments.b) {
				return A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Your data:')
						]));
			} else {
				return A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('No data to show.')
						]));
			}
		}();
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('instance', true),
							_Utils_Tuple2('history', !active)
						]))
				]),
			_List_fromArray(
				[
					active ? $elm$html$Html$text('') : A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'Instance ' + $elm$core$String$fromInt(id + 1))
						])),
					function () {
					var _v0 = instance.J;
					if (_v0.$ === 1) {
						return $elm$html$Html$text('');
					} else {
						var guess = _v0.a;
						var _v1 = A2(adapter.bd.a6, instance.w, guess);
						var correct = _v1.a;
						var desc = _v1.b;
						var guessResultDescription = A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('The guess was '),
									A2(
									$elm$html$Html$strong,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											correct ? 'CORRECT' : 'INCORRECT')
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('guessResultDesc')
										]),
									_List_fromArray(
										[
											A2($elm$html$Html$map, $elm$core$Basics$never, desc)
										]))
								]));
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('guessArea')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$h4,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('Your guess: ')
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('guess')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$map,
											$elm$core$Basics$never,
											A2(adapter.bu.bx, instance.w, guess))
										])),
									guessResultDescription
								]));
					}
				}(),
					dataDesc,
					A3(
					$elm$html$Html$Keyed$node,
					'div',
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('experiments')
						]),
					$author$project$Game$withReverseIds(experiments))
				]));
	});
var $author$project$Game$viewHistory = F2(
	function (adapter, historyList) {
		if (historyList.b) {
			var head = historyList.a;
			var rest = historyList.b;
			var history = A2(
				$author$project$Game$reverseIndexedMap,
				A2($author$project$Game$viewSingleHistory, adapter, false),
				rest);
			var breakElement = function () {
				if (rest.b) {
					return A2(
						$elm$html$Html$h2,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('historyStart')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Previous instances ')
							]));
				} else {
					return $elm$html$Html$text('');
				}
			}();
			var activeId = $elm$core$List$length(historyList) - 1;
			var activeScenario = A4($author$project$Game$viewSingleHistory, adapter, true, activeId, head);
			return A3(
				$elm$html$Html$Keyed$node,
				'div',
				_List_Nil,
				A2(
					$elm$core$List$cons,
					_Utils_Tuple2(
						$elm$core$String$fromInt(activeId),
						activeScenario),
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2('break', breakElement),
						$author$project$Game$withReverseIds(history))));
		} else {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('No instances yet')
					]));
		}
	});
var $author$project$Utils$boolToInt = function (b) {
	return b ? 1 : 0;
};
var $author$project$Game$getResults = F2(
	function (adapter, history) {
		getResults:
		while (true) {
			if (history.b) {
				var head = history.a;
				var rest = history.b;
				var _v1 = head.J;
				if (!_v1.$) {
					var guess = _v1.a;
					var correct = A2(adapter.a6, head.w, guess).a;
					return A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							correct,
							A2($author$project$Game$computeCost, adapter, head)),
						A2($author$project$Game$getResults, adapter, rest));
				} else {
					var $temp$adapter = adapter,
						$temp$history = rest;
					adapter = $temp$adapter;
					history = $temp$history;
					continue getResults;
				}
			} else {
				return _List_Nil;
			}
		}
	});
var $elm$core$Basics$round = _Basics_round;
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $myrho$elm_round$Round$addSign = F2(
	function (signed, str) {
		var isNotZero = A2(
			$elm$core$List$any,
			function (c) {
				return (c !== '0') && (c !== '.');
			},
			$elm$core$String$toList(str));
		return _Utils_ap(
			(signed && isNotZero) ? '-' : '',
			str);
	});
var $elm$core$Char$fromCode = _Char_fromCode;
var $myrho$elm_round$Round$increaseNum = function (_v0) {
	var head = _v0.a;
	var tail = _v0.b;
	if (head === '9') {
		var _v1 = $elm$core$String$uncons(tail);
		if (_v1.$ === 1) {
			return '01';
		} else {
			var headtail = _v1.a;
			return A2(
				$elm$core$String$cons,
				'0',
				$myrho$elm_round$Round$increaseNum(headtail));
		}
	} else {
		var c = $elm$core$Char$toCode(head);
		return ((c >= 48) && (c < 57)) ? A2(
			$elm$core$String$cons,
			$elm$core$Char$fromCode(c + 1),
			tail) : '0';
	}
};
var $elm$core$Basics$isInfinite = _Basics_isInfinite;
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)));
	});
var $elm$core$String$reverse = _String_reverse;
var $myrho$elm_round$Round$splitComma = function (str) {
	var _v0 = A2($elm$core$String$split, '.', str);
	if (_v0.b) {
		if (_v0.b.b) {
			var before = _v0.a;
			var _v1 = _v0.b;
			var after = _v1.a;
			return _Utils_Tuple2(before, after);
		} else {
			var before = _v0.a;
			return _Utils_Tuple2(before, '0');
		}
	} else {
		return _Utils_Tuple2('0', '0');
	}
};
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $myrho$elm_round$Round$toDecimal = function (fl) {
	var _v0 = A2(
		$elm$core$String$split,
		'e',
		$elm$core$String$fromFloat(
			$elm$core$Basics$abs(fl)));
	if (_v0.b) {
		if (_v0.b.b) {
			var num = _v0.a;
			var _v1 = _v0.b;
			var exp = _v1.a;
			var e = A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(
					A2($elm$core$String$startsWith, '+', exp) ? A2($elm$core$String$dropLeft, 1, exp) : exp));
			var _v2 = $myrho$elm_round$Round$splitComma(num);
			var before = _v2.a;
			var after = _v2.b;
			var total = _Utils_ap(before, after);
			var zeroed = (e < 0) ? A2(
				$elm$core$Maybe$withDefault,
				'0',
				A2(
					$elm$core$Maybe$map,
					function (_v3) {
						var a = _v3.a;
						var b = _v3.b;
						return a + ('.' + b);
					},
					A2(
						$elm$core$Maybe$map,
						$elm$core$Tuple$mapFirst($elm$core$String$fromChar),
						$elm$core$String$uncons(
							_Utils_ap(
								A2(
									$elm$core$String$repeat,
									$elm$core$Basics$abs(e),
									'0'),
								total))))) : A3($elm$core$String$padRight, e + 1, '0', total);
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				zeroed);
		} else {
			var num = _v0.a;
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				num);
		}
	} else {
		return '';
	}
};
var $myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if ($elm$core$Basics$isInfinite(fl) || $elm$core$Basics$isNaN(fl)) {
			return $elm$core$String$fromFloat(fl);
		} else {
			var signed = fl < 0;
			var _v0 = $myrho$elm_round$Round$splitComma(
				$myrho$elm_round$Round$toDecimal(
					$elm$core$Basics$abs(fl)));
			var before = _v0.a;
			var after = _v0.b;
			var r = $elm$core$String$length(before) + s;
			var normalized = _Utils_ap(
				A2($elm$core$String$repeat, (-r) + 1, '0'),
				A3(
					$elm$core$String$padRight,
					r,
					'0',
					_Utils_ap(before, after)));
			var totalLen = $elm$core$String$length(normalized);
			var roundDigitIndex = A2($elm$core$Basics$max, 1, r);
			var increase = A2(
				functor,
				signed,
				A3($elm$core$String$slice, roundDigitIndex, totalLen, normalized));
			var remains = A3($elm$core$String$slice, 0, roundDigitIndex, normalized);
			var num = increase ? $elm$core$String$reverse(
				A2(
					$elm$core$Maybe$withDefault,
					'1',
					A2(
						$elm$core$Maybe$map,
						$myrho$elm_round$Round$increaseNum,
						$elm$core$String$uncons(
							$elm$core$String$reverse(remains))))) : remains;
			var numLen = $elm$core$String$length(num);
			var numZeroed = (num === '0') ? num : ((s <= 0) ? _Utils_ap(
				num,
				A2(
					$elm$core$String$repeat,
					$elm$core$Basics$abs(s),
					'0')) : ((_Utils_cmp(
				s,
				$elm$core$String$length(after)) < 0) ? (A3($elm$core$String$slice, 0, numLen - s, num) + ('.' + A3($elm$core$String$slice, numLen - s, numLen, num))) : _Utils_ap(
				before + '.',
				A3($elm$core$String$padRight, s, '0', after))));
			return A2($myrho$elm_round$Round$addSign, signed, numZeroed);
		}
	});
var $myrho$elm_round$Round$round = $myrho$elm_round$Round$roundFun(
	F2(
		function (signed, str) {
			var _v0 = $elm$core$String$uncons(str);
			if (_v0.$ === 1) {
				return false;
			} else {
				if ('5' === _v0.a.a) {
					if (_v0.a.b === '') {
						var _v1 = _v0.a;
						return !signed;
					} else {
						var _v2 = _v0.a;
						return true;
					}
				} else {
					var _v3 = _v0.a;
					var _int = _v3.a;
					return function (i) {
						return ((i > 53) && signed) || ((i >= 53) && (!signed));
					}(
						$elm$core$Char$toCode(_int));
				}
			}
		}));
var $author$project$Utils$safeAverage = function (x) {
	return $elm$core$List$sum(x) / A2(
		$elm$core$Basics$max,
		1.0,
		$elm$core$List$length(x));
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$Game$viewStats = F2(
	function (adapter, scenario) {
		var _v0 = $elm$core$List$unzip(
			A2($author$project$Game$getResults, adapter.bd, scenario.k));
		var correct = _v0.a;
		var cost = _v0.b;
		var correctShort = A2($elm$core$List$take, adapter.ba.bb, correct);
		var propCorrectShort = $author$project$Utils$safeAverage(
			A2($elm$core$List$map, $author$project$Utils$boolToInt, correctShort));
		var nRes = $elm$core$List$length(correct);
		var propCorrect = $author$project$Utils$safeAverage(
			A2($elm$core$List$map, $author$project$Utils$boolToInt, correct));
		var avgCost = $author$project$Utils$safeAverage(cost);
		var costShort = A2($elm$core$List$take, adapter.ba.bb, cost);
		var avgCostShort = $author$project$Utils$safeAverage(costShort);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('stats')
				]),
			A2(
				$elm$core$List$cons,
				A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Results summary')
						])),
				(!nRes) ? _List_fromArray(
					[
						$elm$html$Html$text('No instances completed yet.')
					]) : _List_fromArray(
					[
						A2(
						$elm$html$Html$strong,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('All '),
								$elm$html$Html$text(
								$elm$core$String$fromInt(nRes)),
								$elm$html$Html$text(' instances: ')
							])),
						$elm$html$Html$text(
						A2($myrho$elm_round$Round$round, 1, propCorrect * 100) + ('% correct, avg cost: CZK ' + $elm$core$String$fromInt(
							$elm$core$Basics$round(avgCost)))),
						A2($elm$html$Html$br, _List_Nil, _List_Nil),
						A2(
						$elm$html$Html$strong,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Last '),
								$elm$html$Html$text(
								$elm$core$String$fromInt(adapter.ba.bb)),
								$elm$html$Html$text(' instances: ')
							])),
						(_Utils_cmp(nRes, adapter.ba.bb) > 0) ? $elm$html$Html$text(
						A2($myrho$elm_round$Round$round, 1, propCorrectShort * 100) + ('% correct, avg cost: CZK ' + $elm$core$String$fromInt(
							$elm$core$Basics$round(avgCostShort)))) : $elm$html$Html$text('--')
					])));
	});
var $author$project$Game$view = F2(
	function (adapter, scenario) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('scenario')
				]),
			_List_fromArray(
				[
					A2($author$project$Game$viewStats, adapter, scenario),
					A2($author$project$Game$viewGameControls, adapter, scenario),
					A2($author$project$Game$viewHistory, adapter, scenario.k),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('scenarioFooter')
						]),
					_List_Nil)
				]));
	});
var $author$project$Main$ActivatePage = function (a) {
	return {$: 3, a: a};
};
var $elm$html$Html$a = _VirtualDom_node('a');
var $author$project$Main$pageTitle = function (page) {
	switch (page) {
		case 0:
			return '0: Association';
		case 1:
			return '1: Single relationship';
		default:
			return '2: Two relationships';
	}
};
var $author$project$Main$viewPageSelectionButton = F2(
	function (activePage, page) {
		return A2(
			$author$project$Main$ifActive,
			_Utils_Tuple2(activePage, page),
			_Utils_Tuple2(
				A2(
					$elm$html$Html$h2,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pageSelection'),
							$elm$html$Html$Attributes$class('active')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Main$pageTitle(page))
						])),
				A2(
					$elm$html$Html$h2,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('pageSelection'),
							$elm$html$Html$Attributes$class('inactive')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$a,
							_List_fromArray(
								[
									$elm$html$Html$Events$onClick(
									$author$project$Main$ActivatePage(page))
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									$author$project$Main$pageTitle(page))
								]))
						]))));
	});
var $author$project$Main$viewPageSelection = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		A2(
			$elm$core$List$map,
			$author$project$Main$viewPageSelectionButton(model.L),
			_List_fromArray(
				[0, 1, 2])));
};
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('topContainer')
			]),
		_List_fromArray(
			[
				$author$project$Main$viewPageSelection(model),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('scenarioPage'),
						A2(
						$elm$html$Html$Attributes$style,
						'display',
						A2(
							$author$project$Main$ifActive,
							_Utils_Tuple2(model.L, 0),
							_Utils_Tuple2('block', 'none')))
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$map,
						$author$project$Main$AssocMsg,
						A2($author$project$Game$view, $author$project$Association$adapter, model.ab))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('scenarioPage'),
						A2(
						$elm$html$Html$Attributes$style,
						'display',
						A2(
							$author$project$Main$ifActive,
							_Utils_Tuple2(model.L, 1),
							_Utils_Tuple2('block', 'none')))
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$map,
						$author$project$Main$SingleRel,
						A2($author$project$Game$view, $author$project$SingleRelationship$adapter, model.W))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('scenarioPage'),
						A2(
						$elm$html$Html$Attributes$style,
						'display',
						A2(
							$author$project$Main$ifActive,
							_Utils_Tuple2(model.L, 2),
							_Utils_Tuple2('block', 'none')))
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$map,
						$author$project$Main$TwoRel,
						A2($author$project$Game$view, $author$project$TwoRelationships$adapter, model.Z))
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{
		ba: $author$project$Main$init,
		bn: $elm$core$Basics$always($elm$core$Platform$Sub$none),
		br: $author$project$Main$update,
		bu: $author$project$Main$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));