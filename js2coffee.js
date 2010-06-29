#!/usr/bin/env node

/* Machine translation of JavaScript -> CoffeeScript.
 * 
 * Parser based on cross-browser Javascript parser by Guillaume Lathaud,
 *   http://glathoud.easypagez.com/publications/jscheck/narcissus.jsdef.js
 * 
 * Lathaud's parser is derivative of the SpiderMonkey Narcissus parser,
 *   http://mxr.mozilla.org/mozilla/source/js/narcissus/
 */

/* ***** BEGIN LICENSE BLOCK *****
  * Version: MPL 1.1/GPL 2.0/LGPL 2.1
  *
  * The contents of this file are subject to the Mozilla Public License Version
  * 1.1 (the "License"); you may not use this file except in compliance with
  * the License. You may obtain a copy of the License at
  * http://www.mozilla.org/MPL/
  *
  * Software distributed under the License is distributed on an "AS IS" basis,
  * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  * for the specific language governing rights and limitations under the
  * License.
  *
  * The Original Code is the Narcissus JavaScript engine.
  *
  * The Initial Developer of the Original Code is
  * Brendan Eich <brendan@mozilla.org>.
  * Portions created by the Initial Developer are Copyright (C) 2004
  * the Initial Developer. All Rights Reserved.
  *
  * Contributor(s):
  *
  * Alternatively, the contents of this file may be used under the terms of
  * either the GNU General Public License Version 2 or later (the "GPL"), or
  * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  * in which case the provisions of the GPL or the LGPL are applicable instead
  * of those above. If you wish to allow use of your version of this file only
  * under the terms of either the GPL or the LGPL, and not to allow others to
  * use your version of this file under the terms of the MPL, indicate your
  * decision by deleting the provisions above and replace them with the notice
  * and other provisions required by the GPL or the LGPL. If you do not delete
  * the provisions above, a recipient may use your version of this file under
  * the terms of any one of the MPL, the GPL or the LGPL.
  *
  * ***** END LICENSE BLOCK ***** */

/* Helper functions for shitty JS failings */

function className(obj) {
    if (typeof obj != "object" || obj === null) return false;
    return /(\w+)\(/.exec(obj.constructor.toString())[1];
    }


var narcissus = {};

var jsdef = narcissus.jsdef = {};

var tokens = jsdef.tokens = [
    // End of source.
    "END",
    
    // Operators and punctuators.  Some pair-wise order matters, e.g. (+, -)
    // and (UNARY_PLUS, UNARY_MINUS).
    "\n", ";",
    ",",
    "=",
    "?", ":", "CONDITIONAL",
    "||",
    "&&",
    "|",
    "^",
    "&",
    "==", "!=", "===", "!==",
    "<", "<=", ">=", ">",
    "<<", ">>", ">>>",
    "+", "-",
    "*", "/", "%",
    "!", "~", "UNARY_PLUS", "UNARY_MINUS",
    "++", "--",
    ".",
    "[", "]",
    "{", "}",
    "(", ")",
    
    // Nonterminal tree node type codes.
    "SCRIPT", "BLOCK", "LABEL", "FOR_IN", "CALL", "NEW_WITH_ARGS", "INDEX",
    "ARRAY_INIT", "OBJECT_INIT", "PROPERTY_INIT", "GETTER", "SETTER",
    "GROUP", "LIST",
    
    // Terminals.
    "IDENTIFIER", "NUMBER", "STRING", "REGEXP",
    
    // Keywords.
    "break",
    "case", "catch", "const", "continue",
    "debugger", "default", "delete", "do",
    "else", "enum",
    "false", "finally", "for", "function",
    "if", "in", "instanceof",
    "new", "null",
    "return",
    "switch",
    "this", "throw", "true", "try", "typeof",
    "var", "void",
    "while", "with"
];

// Operator and punctuator mapping from token to tree node type name.
// NB: superstring tokens (e.g., ++) must come before their substring token
// counterparts (+ in the example), so that the opRegExp regular expression
// synthesized from this list makes the longest possible match.
var opTypeNamesArr = jsdef.opTypeNamesArr = [
    [   '\n',   "NEWLINE"],
    [   ';',    "SEMICOLON"],
    [   ',',    "COMMA"],
    [   '?',    "HOOK"],
    [   ':',    "COLON"],
    [   '||',   "OR"],
    [   '&&',   "AND"],
    [   '|',    "BITWISE_OR"],
    [   '^',    "BITWISE_XOR"],
    [   '&',    "BITWISE_AND"],
    [   '===',  "STRICT_EQ"],
    [   '==',   "EQ"],
    [   '=',    "ASSIGN"],
    [   '!==',  "STRICT_NE"],
    [   '!=',   "NE"],
    [   '<<',   "LSH"],
    [   '<=',   "LE"],
    [   '<',    "LT"],
    [   '>>>',  "URSH"],
    [   '>>',   "RSH"],
    [   '>=',   "GE"],
    [   '>',    "GT"],
    [   '++',   "INCREMENT"],
    [   '--',   "DECREMENT"],
    [   '+',    "PLUS"],
    [   '-',    "MINUS"],
    [   '*',    "MUL"],
    [   '/',    "DIV"],
    [   '%',    "MOD"],
    [   '!',    "NOT"],
    [   '~',    "BITWISE_NOT"],
    [   '.',    "DOT"],
    [   '[',    "LEFT_BRACKET"],
    [   ']',    "RIGHT_BRACKET"],
    [   '{',    "LEFT_CURLY"],
    [   '}',    "RIGHT_CURLY"],
    [   '(',    "LEFT_PAREN"],
    [   ')',    "RIGHT_PAREN"]
];

var opTypeNames = jsdef.opTypeNames = (function () {
    var ret = {}, x;
    for (var i = 0; i < opTypeNamesArr.length; i++){
        x = opTypeNamesArr[i];
        ret[x[0]] = x[1];
    }
    return ret;
})();


// Define const END, etc., based on the token names.  Also map name to index.

var keywords = jsdef.keywords = (function () {

    // Hash of keyword identifier to tokens index.  NB: we must null __proto__ to
    // avoid toString, etc. namespace pollution. 
    //  var _keywords = {__proto__: null};

    // G. Lathoud's addition: This works however only on SpiderMonkey and the like,
    // so let's resort to a more basic approach with hasOwnProperty (see further below).
    // (this helps on Rhino 1.6).

    var _keywords = {};
    
    for (var i = 0, j = tokens.length; i < j; i++) {


        var a_const;
        
        var t = tokens[i];
        if (/^[a-z]/.test(t)) {
            a_const = t.toUpperCase();
            _keywords[t] = i;
        } else {
            a_const = (/^\W/.test(t) ? opTypeNames[t] : t);
        }
        
        jsdef[a_const] = i >> 0;
        
        tokens[t] = i;
    }

    return function (/*string*/id) {
        return _keywords.hasOwnProperty(id) && _keywords[id];
    };
})();

// Map assignment operators to their indexes in the tokens array.
var assignOps = jsdef.assignOps = ['|', '^', '&', '<<', '>>', '>>>', '+', '-', '*', '/', '%'];

for (var i = 0, j = assignOps.length; i < j; i++) {
    var t = assignOps[i];
    assignOps[t] = tokens[t];
}

if (typeof from !== 'undefined')
    from.req.done('narcissus.jsdef', true);









    var jsdef = narcissus.jsdef;

    // Build a regexp that recognizes operators and punctuators (except newline).
    var opRegExpSrc = (function () {
        var ret = "^", a, i;
        for (var a = 0; a < jsdef.opTypeNamesArr.length; a++){
            i = jsdef.opTypeNamesArr[a][0];
            if (i == '\n')
                continue;
            if (ret != "^")
                ret += "|^";
            ret += i.replace(/[?|^&(){}\[\]+\-*\/\.]/g, "\\$&");
        }
        return ret;
    })();

    var opRegExp = new RegExp(opRegExpSrc);

    // A regexp to match floating point literals (but not integer literals).
    var fpRegExp = /^\d+\.\d*(?:[eE][-+]?\d+)?|^\d+(?:\.\d*)?[eE][-+]?\d+|^\.\d+(?:[eE][-+]?\d+)?/;

    // A regexp to match regexp literals.
    var reRegExp = /^\/((?:\\.|\[(?:\\.|[^\]])*\]|[^\/])+)\/([gimy]*)/;

    function Tokenizer(s, f, l) {
        this.cursor = 0;
        this.source = String(s);
        this.tokens = [];
        this.tokenIndex = 0;
        this.lookahead = 0;
        this.scanNewlines = false;
        this.scanOperand = true;
        this.filename = f || "";
        this.lineno = l || 1;
    }

    Tokenizer.prototype = {
        input: function() {
            return this.source.substring(this.cursor);
        },

        done: function() {
            return this.peek() == jsdef.END;
        },

        token: function() {
            return this.tokens[this.tokenIndex];
        },

        match: function (tt) {
            return this.get() == tt || this.unget();
        },

        mustMatch: function (tt) {
            if (!this.match(tt))
                throw this.newSyntaxError("Missing " + jsdef.tokens[tt].toLowerCase());
            return this.token();
        },

        peek: function () {
            var tt, next;
            if (this.lookahead) {
                next = this.tokens[(this.tokenIndex + this.lookahead) & 3];
                if (this.scanNewlines && next.lineno != this.lineno)
                    tt = jsdef.NEWLINE;
                else
                    tt = next.type;
            } else {
                tt = this.get();
                this.unget();
            }
            return tt;
        },

        peekOnSameLine: function () {
            this.scanNewlines = true;
            var tt = this.peek();
            this.scanNewlines = false;
            return tt;
        },

        get: function () {
            var token;
            while (this.lookahead) {
                --this.lookahead;
                this.tokenIndex = (this.tokenIndex + 1) & 3;
                token = this.tokens[this.tokenIndex];
                if (token.type != jsdef.NEWLINE || this.scanNewlines) {
                    return token.type;
                }
            }

            for (;;) {
                var input = this.input();
                var match = (this.scanNewlines ? /^[ \t]+/ : /^\s+/).exec(input);
                if (match) {
                    var spaces = match[0];
                    this.cursor += spaces.length;
                    var newlines = spaces.match(/\n/g);
                    if (newlines)
                        this.lineno += newlines.length;
                    input = this.input();
                }

                if (!(match = /^\/(?:\*(?:[\S\s])*?\*\/|\/.*)/.exec(input)))
                    break;
                var comment = match[0];
                this.cursor += comment.length;
                newlines = comment.match(/\n/g);
                if (newlines)
                    this.lineno += newlines.length;
            }

            this.tokenIndex = (this.tokenIndex + 1) & 3;
            token = this.tokens[this.tokenIndex];
            if (!token)
                this.tokens[this.tokenIndex] = token = {};

            if (!input) {
                return token.type = jsdef.END;
            }

            if ((match = fpRegExp.exec(input))) {
                token.type = jsdef.NUMBER;
                token.value = parseFloat(match[0]);
            } else if ((match = /^0[xX][\da-fA-F]+|^0[0-7]*|^\d+/.exec(input))) {
                token.type = jsdef.NUMBER;
                token.value = parseInt(match[0]);
            } else if ((match = /^[$_\w]+/.exec(input))) {       // FIXME no ES3 unicode
                var id = match[0];
                token.type = jsdef.keywords(id) || jsdef.IDENTIFIER;
                token.value = id;
            } else if ((match = /^"(?:\\.|[^"])*"|^'(?:\\.|[^'])*'/.exec(input))) { // "){
                token.type = jsdef.STRING;
            token.value = eval(match[0]);
        } else if (this.scanOperand && (match = reRegExp.exec(input))) {
            token.type = jsdef.REGEXP;
            token.value = new RegExp(match[1], match[2]);
        } else if ((match = opRegExp.exec(input))) {
            var op = match[0];
            if (jsdef.assignOps[op] && input[op.length] == '=') {
                token.type = jsdef.ASSIGN;
                token.assignOp = jsdef[jsdef.opTypeNames[op]];
                match[0] += '=';
            } else {
                token.type = jsdef[jsdef.opTypeNames[op]];
                if (this.scanOperand &&
                    (token.type == jsdef.PLUS || token.type == jsdef.MINUS)) {
                    token.type += jsdef.UNARY_PLUS - jsdef.PLUS;
                }
                token.assignOp = null;
            }
            token.value = op;
        } else if (this.scanNewlines && (match = /^\n/.exec(input))) {
            token.type = jsdef.NEWLINE;
        } else {
            throw this.newSyntaxError("Illegal token");
        }

        token.start = this.cursor;
        this.cursor += match[0].length;
        token.end = this.cursor;
        token.lineno = this.lineno;

        return token.type;
    },

    unget: function () {
        if (++this.lookahead == 4) throw "PANIC: too much lookahead!";
        this.tokenIndex = (this.tokenIndex - 1) & 3;
    },

    newSyntaxError: function (m) {
        var e = new SyntaxError(m + ', filename:' + this.filename + ', lineno:' + this.lineno);
        e.source = this.source;
        e.cursor = this.cursor;
        return e;
    }
};

function CompilerContext(inFunction) {
    this.inFunction = inFunction;
    this.stmtStack = [];
    this.funDecls = [];
    this.varDecls = [];
}

var CCp = CompilerContext.prototype;
CCp.bracketLevel = CCp.curlyLevel = CCp.parenLevel = CCp.hookLevel = 0;
CCp.ecmaStrictMode = CCp.inForLoopInit = false;

function Script(t, x) {
    var n = Statements(t, x);
    n.type = jsdef.SCRIPT;
    n.funDecls = x.funDecls;
    n.varDecls = x.varDecls;
    return n;
}

// Node extends Array, which we extend slightly with a top-of-stack method.
Array.prototype.top = function () {
    return this.length && this[this.length-1];
};

function Node(t, type) {
    var token = t.token();
    if (token) {
        this.type = type || token.type;
        this.value = token.value;
        this.lineno = token.lineno;
        this.start = token.start;
        this.end = token.end;
    } else {
        this.type = type;
        this.lineno = t.lineno;
    }
    this.tokenizer = t;

    for (var i = 2; i < arguments.length; i++)
        this.push(arguments[i]);
}

var Np = Node.prototype = new Array;
Np.constructor = Node;
Np.toSource = Object.prototype.toSource;

// Always use push to add operands to an expression, to update start and end.
Np.push = function (kid) {
    if (!kid) 
        throw this.tokenizer.newSyntaxError('Empty child expression!');
    if (kid.start < this.start)
        this.start = kid.start;
    if (this.end < kid.end)
        this.end = kid.end;
    return Array.prototype.push.call(this, kid);
};

Node.indentLevel = 0;

function tokenstr(tt) {
    var t = jsdef.tokens[tt];
    return /^\W/.test(t) ? jsdef.opTypeNames[t] : t.toUpperCase();
}

Np.toString = function () {
    var a = [];
    for (var i in this) {
        if (this.hasOwnProperty(i) && i != 'type' && i != 'target')
            a.push({id: i, value: this[i]});
    }
    a.sort(function (a,b) { return (a.id < b.id) ? -1 : 1; });
    jsdef.INDENTATION = "    ";
    var n = ++Node.indentLevel;
    var s = "{\n" + jsdef.INDENTATION.repeat(n) + "type: " + tokenstr(this.type);
    for (i = 0; i < a.length; i++)
        s += ",\n" + jsdef.INDENTATION.repeat(n) + a[i].id + ": " + a[i].value;
    n = --Node.indentLevel;
    s += "\n" + jsdef.INDENTATION.repeat(n) + "}";
    return s;
};

Np.getSource = function () {
    return this.tokenizer.source.slice(this.start, this.end);
};

Np.filename = function () { return this.tokenizer.filename; };

String.prototype.repeat = function (n) {
    var s = "", t = this + s;
    while (--n >= 0)
        s += t;
    return s;
};

// Statement stack and nested statement handler.
function nest(t, x, node, func, end) {
    x.stmtStack.push(node);
    var n = func(t, x);
    x.stmtStack.pop();
    end && t.mustMatch(end);
    return n;
}

function Statements(t, x) {
    var n = new Node(t, jsdef.BLOCK);
    x.stmtStack.push(n);
    while (!t.done() && t.peek() != jsdef.RIGHT_CURLY)
        n.push(Statement(t, x));
    x.stmtStack.pop();
    return n;
}

function Block(t, x) {
    t.mustMatch(jsdef.LEFT_CURLY);
    var n = Statements(t, x);
    t.mustMatch(jsdef.RIGHT_CURLY);
    return n;
}

var DECLARED_FORM = 0, EXPRESSED_FORM = 1, STATEMENT_FORM = 2;

function Statement(t, x) {
    var i, label, n, n2, ss, tt = t.get();

    // Cases for statements ending in a right curly return early, avoiding the
    // common semicolon insertion magic after this switch.
    switch (tt) {
    case jsdef.FUNCTION:
        return FunctionDefinition(t, x, true,
                                  (x.stmtStack.length > 1)
                                  ? STATEMENT_FORM
                                  : DECLARED_FORM);

    case jsdef.LEFT_CURLY:
        n = Statements(t, x);
        t.mustMatch(jsdef.RIGHT_CURLY);
        return n;

    case jsdef.IF:
        n = new Node(t);
        n.condition = ParenExpression(t, x);
        x.stmtStack.push(n);
        n.thenPart = Statement(t, x);
        n.elsePart = t.match(jsdef.ELSE) ? Statement(t, x) : null;
        x.stmtStack.pop();
        return n;

    case jsdef.SWITCH:
        n = new Node(t);
        t.mustMatch(jsdef.LEFT_PAREN);
        n.discriminant = Expression(t, x);
        t.mustMatch(jsdef.RIGHT_PAREN);
        n.cases = [];
        n.defaultIndex = -1;
        x.stmtStack.push(n);
        t.mustMatch(jsdef.LEFT_CURLY);
        while ((tt = t.get()) != jsdef.RIGHT_CURLY) {
            switch (tt) {
            case jsdef.DEFAULT:
                if (n.defaultIndex >= 0)
                    throw t.newSyntaxError("More than one switch default");
                // FALL THROUGH
            case jsdef.CASE:
                n2 = new Node(t);
                if (tt == jsdef.DEFAULT)
                    n.defaultIndex = n.cases.length;
                else
                    n2.caseLabel = Expression(t, x, jsdef.COLON);
                break;
            default:
                throw t.newSyntaxError("Invalid switch case");
            }
            t.mustMatch(jsdef.COLON);
            n2.statements = new Node(t, jsdef.BLOCK);
            while ((tt=t.peek()) != jsdef.CASE && tt != jsdef.DEFAULT && tt != jsdef.RIGHT_CURLY)
                n2.statements.push(Statement(t, x));
            n.cases.push(n2);
        }
        x.stmtStack.pop();
        return n;

    case jsdef.FOR:
        n = new Node(t);
        n.isLoop = true;
        t.mustMatch(jsdef.LEFT_PAREN);
        if ((tt = t.peek()) != jsdef.SEMICOLON) {
            x.inForLoopInit = true;
            if (tt == jsdef.VAR || tt == jsdef.CONST) {
                t.get();
                n2 = Variables(t, x);
            } else {
                n2 = Expression(t, x);
            }
            x.inForLoopInit = false;
        }
        if (n2 && t.match(jsdef.IN)) {
            n.type = jsdef.FOR_IN;
            if (n2.type == jsdef.VAR) {
                if (n2.length != 1) {
                    throw new SyntaxError("Invalid for..in left-hand side",
                                          t.filename, n2.lineno);
                }

                // NB: n2[0].type == jsdef.IDENTIFIER and n2[0].value == n2[0].name.
                n.iterator = n2[0];
                n.varDecl = n2;
            } else {
                n.iterator = n2;
                n.varDecl = null;
            }
            n.object = Expression(t, x);
        } else {
            n.setup = n2 || null;
            t.mustMatch(jsdef.SEMICOLON);
            n.condition = (t.peek() == jsdef.SEMICOLON) ? null : Expression(t, x);
            t.mustMatch(jsdef.SEMICOLON);
            n.update = (t.peek() == jsdef.RIGHT_PAREN) ? null : Expression(t, x);
        }
        t.mustMatch(jsdef.RIGHT_PAREN);
        n.body = nest(t, x, n, Statement);
        return n;

    case jsdef.WHILE:
        n = new Node(t);
        n.isLoop = true;
        n.condition = ParenExpression(t, x);
        n.body = nest(t, x, n, Statement);
        return n;

    case jsdef.DO:
        n = new Node(t);
        n.isLoop = true;
        n.body = nest(t, x, n, Statement, jsdef.WHILE);
        n.condition = ParenExpression(t, x);
        if (!x.ecmaStrictMode) {
            // <script language="JavaScript"> (without version hints) may need
            // automatic semicolon insertion without a newline after do-while.
            // See http://bugzilla.mozilla.org/show_bug.cgi?id=238945.
            t.match(jsdef.SEMICOLON);
            return n;
        }
        break;

    case jsdef.BREAK:
    case jsdef.CONTINUE:
        n = new Node(t);
        if (t.peekOnSameLine() == jsdef.IDENTIFIER) {
            t.get();
            n.label = t.token().value;
        }
        ss = x.stmtStack;
        i = ss.length;
        label = n.label;
        if (label) {
            do {
                if (--i < 0)
                    throw t.newSyntaxError("Label not found");
            } while (ss[i].label != label);
        } else {
            do {
                if (--i < 0) {
                    throw t.newSyntaxError("Invalid " + ((tt == jsdef.BREAK)
                                                         ? "break"
                                                         : "continue"));
                }
            } while (!ss[i].isLoop && (tt != jsdef.BREAK || ss[i].type != jsdef.SWITCH));
        }
        n.target = ss[i];
        break;

    case jsdef.TRY:
        n = new Node(t);
        n.tryBlock = Block(t, x);
        n.catchClauses = [];
        while (t.match(jsdef.CATCH)) {
            n2 = new Node(t);
            t.mustMatch(jsdef.LEFT_PAREN);
            n2.varName = t.mustMatch(jsdef.IDENTIFIER).value;
            if (t.match(jsdef.IF)) {
                if (x.ecmaStrictMode)
                    throw t.newSyntaxError("Illegal catch guard");
                if (n.catchClauses.length && !n.catchClauses.top().guard)
                    throw t.newSyntaxError("Guarded catch after unguarded");
                n2.guard = Expression(t, x);
            } else {
                n2.guard = null;
            }
            t.mustMatch(jsdef.RIGHT_PAREN);
            n2.block = Block(t, x);
            n.catchClauses.push(n2);
        }
        if (t.match(jsdef.FINALLY))
            n.finallyBlock = Block(t, x);
        if (!n.catchClauses.length && !n.finallyBlock)
            throw t.newSyntaxError("Invalid try statement");
        return n;

    case jsdef.CATCH:
    case jsdef.FINALLY:
        throw t.newSyntaxError(jsdef.tokens[tt] + " without preceding try");

    case jsdef.THROW:
        n = new Node(t);
        n.exception = Expression(t, x);
        break;

    case jsdef.RETURN:
        if (!x.inFunction)
            throw t.newSyntaxError("Invalid return");
        n = new Node(t);
        tt = t.peekOnSameLine();
        if (tt != jsdef.END && tt != jsdef.NEWLINE && tt != jsdef.SEMICOLON && tt != jsdef.RIGHT_CURLY)
            n.value = Expression(t, x);
        break;

    case jsdef.WITH:
        n = new Node(t);
        n.object = ParenExpression(t, x);
        n.body = nest(t, x, n, Statement);
        return n;

    case jsdef.VAR:
    case jsdef.CONST:
        n = Variables(t, x);
        break;

    case jsdef.DEBUGGER:
        n = new Node(t);
        break;

    case jsdef.NEWLINE:
    case jsdef.SEMICOLON:
        n = new Node(t, jsdef.SEMICOLON);
        n.expression = null;
        return n;

    default:
        if (tt == jsdef.IDENTIFIER) {
            t.scanOperand = false;
            tt = t.peek();
            t.scanOperand = true;
            if (tt == jsdef.COLON) {
                label = t.token().value;
                ss = x.stmtStack;
                for (i = ss.length-1; i >= 0; --i) {
                    if (ss[i].label == label)
                        throw t.newSyntaxError("Duplicate label");
                }
                t.get();
                n = new Node(t, jsdef.LABEL);
                n.label = label;
                n.statement = nest(t, x, n, Statement);
                return n;
            }
        }

        n = new Node(t, jsdef.SEMICOLON);
        t.unget();
        n.expression = Expression(t, x);
        n.end = n.expression.end;
        break;
    }

    if (t.lineno == t.token().lineno) {
        tt = t.peekOnSameLine();
        if (tt != jsdef.END && tt != jsdef.NEWLINE && tt != jsdef.SEMICOLON && tt != jsdef.RIGHT_CURLY)
            throw t.newSyntaxError("Missing ; before statement");
    }
    t.match(jsdef.SEMICOLON);
    return n;
}

function FunctionDefinition(t, x, requireName, functionForm) {
    var f = new Node(t);
    if (f.type != jsdef.FUNCTION)
        f.type = (f.value == "get") ? jsdef.GETTER : jsdef.SETTER;
    if (t.match(jsdef.IDENTIFIER))
        f.name = t.token().value;
    else if (requireName)
        throw t.newSyntaxError("Missing function identifier");

    t.mustMatch(jsdef.LEFT_PAREN);
    f.params = [];
    var tt;
    while ((tt = t.get()) != jsdef.RIGHT_PAREN) {
        if (tt != jsdef.IDENTIFIER)
            throw t.newSyntaxError("Missing formal parameter");
        f.params.push(t.token().value);
        if (t.peek() != jsdef.RIGHT_PAREN)
            t.mustMatch(jsdef.COMMA);
    }

    t.mustMatch(jsdef.LEFT_CURLY);
    var x2 = new CompilerContext(true);
    f.body = Script(t, x2);
    t.mustMatch(jsdef.RIGHT_CURLY);
    f.end = t.token().end;

    f.functionForm = functionForm;
    if (functionForm == DECLARED_FORM)
        x.funDecls.push(f);
    return f;
}

function Variables(t, x) {
    var n = new Node(t);
    do {
        t.mustMatch(jsdef.IDENTIFIER);
        var n2 = new Node(t);
        n2.name = n2.value;
        if (t.match(jsdef.ASSIGN)) {
            if (t.token().assignOp)
                throw t.newSyntaxError("Invalid variable initialization");
            n2.initializer = Expression(t, x, jsdef.COMMA);
        }
        n2.readOnly = (n.type == jsdef.CONST);
        n.push(n2);
        x.varDecls.push(n2);
    } while (t.match(jsdef.COMMA));
    return n;
}

function ParenExpression(t, x) {
    t.mustMatch(jsdef.LEFT_PAREN);
    var n = Expression(t, x);
    t.mustMatch(jsdef.RIGHT_PAREN);
    return n;
}

var opPrecedence = {
    SEMICOLON: 0,
    COMMA: 1,
    ASSIGN: 2, HOOK: 2, COLON: 2,
    // The above all have to have the same precedence, see bug 330975.
    OR: 4,
    AND: 5,
    BITWISE_OR: 6,
    BITWISE_XOR: 7,
    BITWISE_AND: 8,
    EQ: 9, NE: 9, STRICT_EQ: 9, STRICT_NE: 9,
    LT: 10, LE: 10, GE: 10, GT: 10, IN: 10, INSTANCEOF: 10,
    LSH: 11, RSH: 11, URSH: 11,
    PLUS: 12, MINUS: 12,
    MUL: 13, DIV: 13, MOD: 13,
    DELETE: 14, VOID: 14, TYPEOF: 14, // PRE_INCREMENT: 14, PRE_DECREMENT: 14,
    NOT: 14, BITWISE_NOT: 14, UNARY_PLUS: 14, UNARY_MINUS: 14,
    INCREMENT: 15, DECREMENT: 15,     // postfix
    NEW: 16,
    DOT: 17
};

// Map operator type code to precedence.
for (var i in opPrecedence)
    opPrecedence[jsdef[i]] = opPrecedence[i];

var opArity = {
    COMMA: -2,
    ASSIGN: 2,
    HOOK: 3,
    OR: 2,
    AND: 2,
    BITWISE_OR: 2,
    BITWISE_XOR: 2,
    BITWISE_AND: 2,
    EQ: 2, NE: 2, STRICT_EQ: 2, STRICT_NE: 2,
    LT: 2, LE: 2, GE: 2, GT: 2, IN: 2, INSTANCEOF: 2,
    LSH: 2, RSH: 2, URSH: 2,
    PLUS: 2, MINUS: 2,
    MUL: 2, DIV: 2, MOD: 2,
    DELETE: 1, VOID: 1, TYPEOF: 1,  // PRE_INCREMENT: 1, PRE_DECREMENT: 1,
    NOT: 1, BITWISE_NOT: 1, UNARY_PLUS: 1, UNARY_MINUS: 1,
    INCREMENT: 1, DECREMENT: 1,     // postfix
    NEW: 1, NEW_WITH_ARGS: 2, DOT: 2, INDEX: 2, CALL: 2,
    ARRAY_INIT: 1, OBJECT_INIT: 1, GROUP: 1
};

// Map operator type code to arity.
for (var i in opArity)
    opArity[jsdef[i]] = opArity[i];

function Expression(t, x, stop) {
    var n, id, tt, operators = [], operands = [];
    var bl = x.bracketLevel, cl = x.curlyLevel, pl = x.parenLevel,
    hl = x.hookLevel;

    function reduce() {
        var n = operators.pop();
        var op = n.type;
        var arity = opArity[op];
        if (arity == -2) {
            // Flatten left-associative trees.
            var left = operands.length >= 2 && operands[operands.length-2];
            if (left.type == op) {
                var right = operands.pop();
                left.push(right);
                return left;
            }
            arity = 2;
        }

        // Always use push to add operands to n, to update start and end.
        var a = operands.splice(operands.length - arity, arity);
        for (var i = 0; i < arity; i++)
            n.push(a[i]);

        // Include closing bracket or postfix operator in [start,end).
        if (n.end < t.token().end)
            n.end = t.token().end;

        operands.push(n);
        return n;
    }

    loop:
    while ((tt = t.get()) != jsdef.END) {
        if (tt == stop &&
            x.bracketLevel == bl && x.curlyLevel == cl && x.parenLevel == pl &&
            x.hookLevel == hl) {
            // Stop only if tt matches the optional stop parameter, and that
            // token is not quoted by some kind of bracket.
            break;
        }
        switch (tt) {
        case jsdef.SEMICOLON:
            // NB: cannot be empty, Statement handled that.
            break loop;

        case jsdef.ASSIGN:
        case jsdef.HOOK:
        case jsdef.COLON:
            if (t.scanOperand)
                break loop;
            // Use >, not >=, for right-associative jsdef.ASSIGN and jsdef.HOOK/jsdef.COLON.
            while (opPrecedence[operators.top().type] > opPrecedence[tt] ||
                   (tt == jsdef.COLON && operators.top().type == jsdef.ASSIGN)) {
                reduce();
            }
            if (tt == jsdef.COLON) {
                n = operators.top();
                if (n.type != jsdef.HOOK)
                    throw t.newSyntaxError("Invalid label");
                --x.hookLevel;
            } else {
                operators.push(new Node(t));
                if (tt == jsdef.ASSIGN)
                    operands.top().assignOp = t.token().assignOp;
                else
                    ++x.hookLevel;      // tt == jsdef.HOOK
            }
            t.scanOperand = true;
            break;

        case jsdef.IN:
            // An in operator should not be parsed if we're parsing the head of
            // a for (...) loop, unless it is in the then part of a conditional
            // expression, or parenthesized somehow.
            if (x.inForLoopInit && !x.hookLevel &&
                !x.bracketLevel && !x.curlyLevel && !x.parenLevel) {
                break loop;
            }
            // FALL THROUGH
        case jsdef.COMMA:
            // Treat comma as left-associative so reduce can fold left-heavy
            // jsdef.COMMA trees into a single array.
            // FALL THROUGH
        case jsdef.OR:
        case jsdef.AND:
        case jsdef.BITWISE_OR:
        case jsdef.BITWISE_XOR:
        case jsdef.BITWISE_AND:
        case jsdef.EQ: case jsdef.NE: case jsdef.STRICT_EQ: case jsdef.STRICT_NE:
        case jsdef.LT: case jsdef.LE: case jsdef.GE: case jsdef.GT:
        case jsdef.INSTANCEOF:
        case jsdef.LSH: case jsdef.RSH: case jsdef.URSH:
        case jsdef.PLUS: case jsdef.MINUS:
        case jsdef.MUL: case jsdef.DIV: case jsdef.MOD:
        case jsdef.DOT:
            if (t.scanOperand)
                break loop;
            while (opPrecedence[operators.top().type] >= opPrecedence[tt])
                reduce();
            if (tt == jsdef.DOT) {
                t.mustMatch(jsdef.IDENTIFIER);
                operands.push(new Node(t, jsdef.DOT, operands.pop(), new Node(t)));
            } else {
                operators.push(new Node(t));
                t.scanOperand = true;
            }
            break;

        case jsdef.DELETE: case jsdef.VOID: case jsdef.TYPEOF:
        case jsdef.NOT: case jsdef.BITWISE_NOT: case jsdef.UNARY_PLUS: case jsdef.UNARY_MINUS:
        case jsdef.NEW:
            if (!t.scanOperand)
                break loop;
            operators.push(new Node(t));
            break;

        case jsdef.INCREMENT: case jsdef.DECREMENT:
            if (t.scanOperand) {
                operators.push(new Node(t));  // prefix increment or decrement
            } else {
                // Don't cross a line boundary for postfix {in,de}crement.
                if (t.tokens[(t.tokenIndex + t.lookahead - 1) & 3].lineno !=
                    t.lineno) {
                    break loop;
                }

                // Use >, not >=, so postfix has higher precedence than prefix.
                while (opPrecedence[operators.top().type] > opPrecedence[tt])
                    reduce();
                n = new Node(t, tt, operands.pop());
                n.postfix = true;
                operands.push(n);
            }
            break;

        case jsdef.FUNCTION:
            if (!t.scanOperand)
                break loop;
            operands.push(FunctionDefinition(t, x, false, EXPRESSED_FORM));
            t.scanOperand = false;
            break;

        case jsdef.NULL: case jsdef.THIS: case jsdef.TRUE: case jsdef.FALSE:
        case jsdef.IDENTIFIER: case jsdef.NUMBER: case jsdef.STRING: case jsdef.REGEXP:
            if (!t.scanOperand)
                break loop;
            operands.push(new Node(t));
            t.scanOperand = false;
            break;

        case jsdef.LEFT_BRACKET:
            if (t.scanOperand) {
                // Array initialiser.  Parse using recursive descent, as the
                // sub-grammar here is not an operator grammar.
                n = new Node(t, jsdef.ARRAY_INIT);
                while ((tt = t.peek()) != jsdef.RIGHT_BRACKET) {
                    if (tt == jsdef.COMMA) {
                        t.get();
                        n.push(null);
                        continue;
                    }
                    n.push(Expression(t, x, jsdef.COMMA));
                    if (!t.match(jsdef.COMMA))
                        break;
                }
                t.mustMatch(jsdef.RIGHT_BRACKET);
                operands.push(n);
                t.scanOperand = false;
            } else {
                // Property indexing operator.
                operators.push(new Node(t, jsdef.INDEX));
                t.scanOperand = true;
                ++x.bracketLevel;
            }
            break;

        case jsdef.RIGHT_BRACKET:
            if (t.scanOperand || x.bracketLevel == bl)
                break loop;
            while (reduce().type != jsdef.INDEX)
                continue;
            --x.bracketLevel;
            break;

        case jsdef.LEFT_CURLY:
            if (!t.scanOperand)
                break loop;
            // Object initialiser.  As for array initialisers (see above),
            // parse using recursive descent.
            ++x.curlyLevel;
            n = new Node(t, jsdef.OBJECT_INIT);
            object_init:
            if (!t.match(jsdef.RIGHT_CURLY)) {
                do {
                    tt = t.get();
                    if ((t.token().value == "get" || t.token().value == "set") &&
                        t.peek() == jsdef.IDENTIFIER) {
                        if (x.ecmaStrictMode)
                            throw t.newSyntaxError("Illegal property accessor");
                        n.push(FunctionDefinition(t, x, true, EXPRESSED_FORM));
                    } else {
                        switch (tt) {
                        case jsdef.IDENTIFIER:
                        case jsdef.NUMBER:
                        case jsdef.STRING:
                            id = new Node(t);
                            break;
                        case jsdef.RIGHT_CURLY:
                            if (x.ecmaStrictMode)
                                throw t.newSyntaxError("Illegal trailing ,");
                            break object_init;
                        default:
                            throw t.newSyntaxError("Invalid property name");
                        }
                        t.mustMatch(jsdef.COLON);
                        n.push(new Node(t, jsdef.PROPERTY_INIT, id,
                                        Expression(t, x, jsdef.COMMA)));
                    }
                } while (t.match(jsdef.COMMA));
                t.mustMatch(jsdef.RIGHT_CURLY);
            }
            operands.push(n);
            t.scanOperand = false;
            --x.curlyLevel;
            break;

        case jsdef.RIGHT_CURLY:
            if (!t.scanOperand && x.curlyLevel != cl)
                throw "PANIC: right curly botch";
            break loop;

        case jsdef.LEFT_PAREN:
            if (t.scanOperand) {
                operators.push(new Node(t, jsdef.GROUP));
            } else {
                while (opPrecedence[operators.top().type] > opPrecedence[jsdef.NEW])
                    reduce();

                // Handle () now, to regularize the n-ary case for n > 0.
                // We must set scanOperand in case there are arguments and
                // the first one is a regexp or unary+/-.
                n = operators.top();
                t.scanOperand = true;
                if (t.match(jsdef.RIGHT_PAREN)) {
                    if (n.type == jsdef.NEW) {
                        --operators.length;
                        n.push(operands.pop());
                    } else {
                        n = new Node(t, jsdef.CALL, operands.pop(),
                                     new Node(t, jsdef.LIST));
                    }
                    operands.push(n);
                    t.scanOperand = false;
                    break;
                }
                if (n.type == jsdef.NEW)
                    n.type = jsdef.NEW_WITH_ARGS;
                else
                    operators.push(new Node(t, jsdef.CALL));
            }
            ++x.parenLevel;
            break;

        case jsdef.RIGHT_PAREN:
            if (t.scanOperand || x.parenLevel == pl)
                break loop;
            while ((tt = reduce().type) != jsdef.GROUP && tt != jsdef.CALL &&
                   tt != jsdef.NEW_WITH_ARGS) {
                continue;
            }
            if (tt != jsdef.GROUP) {
                n = operands.top();
                if (n[1].type != jsdef.COMMA)
                    n[1] = new Node(t, jsdef.LIST, n[1]);
                else
                    n[1].type = jsdef.LIST;
            }
            --x.parenLevel;
            break;

            // Automatic semicolon insertion means we may scan across a newline
            // and into the beginning of another statement.  If so, break out of
            // the while loop and let the t.scanOperand logic handle errors.
        default:
            break loop;
        }
    }

    if (x.hookLevel != hl)
        throw t.newSyntaxError("Missing : after ?");
    if (x.parenLevel != pl)
        throw t.newSyntaxError("Missing ) in parenthetical");
    if (x.bracketLevel != bl)
        throw t.newSyntaxError("Missing ] in index expression");
    if (t.scanOperand)
        throw t.newSyntaxError("Missing operand");

    // Resume default mode, scanning for operands, not operators.
    t.scanOperand = true;
    t.unget();
    while (operators.length)
        reduce();
    return operands.pop();
}

function jsparse(s, f, l) {
    var t = new Tokenizer(s, f, l);
    var x = new CompilerContext(false);
    var n = Script(t, x);
    if (!t.done())
        throw t.newSyntaxError("Syntax error");
    return n;
}

narcissus.jsparse = jsparse;



/* END OF NARCISSUS; START OF JS2COFFEE */



sys = require("sys");
fs = require("fs");

function ungroup_condition(t) {
    if (t.condition.type == jsdef.GROUP) {
        // don't need to group a whole condition
        t.condition = t.condition[0];
        }
    }


/* Helper parsing functions */

function listChildren(from, me) {
    var into = [];
    for(var i = 0; i < from.length; i++) {
        into.push(narcissus2object(from[i], me));
        }
    return into;
    }

/* Constructors */

function JsSemicolon    () {}
function JsComma        () {}
function JsAssign       () {}
function JsHook         () {}
function JsOr           () {}
function JsAnd          () {}
function JsBitwiseOr    () {}
function JsBitwiseAnd   () {}
function JsEq           () {}
function JsNe           () {}
function JsStrictEq     () {}
function JsStrictNe     () {}
function JsLt           () {}
function JsLe           () {}
function JsGe           () {}
function JsGt           () {}
function JsRightShift   () {}
function JsPlus         () {}
function JsMinus        () {}
function JsMul          () {}
function JsDiv          () {}
function JsMod          () {}
function JsNot          () {}
function JsUnaryPlus    () {}
function JsUnaryMinus   () {}
function JsDecrement    () {}
function JsIncrement    () {}
function JsDot          () {}
function JsScript       () {}
function JsBlock        () {}
function JsForIn        () {}
function JsCall         () {}
function JsNewWithArgs  () {}
function JsIndex        () {}
function JsArrayInit    () {}
function JsObjectInit   () {}
function JsPropertyInit () {}
function JsGroup        () {}
function JsList         () {}
function JsIdentifier   () {}
function JsNumber       () {}
function JsString       () {}
function JsRegExp       () {}
function JsBreak        () {}
function JsCase         () {}
function JsCatch        () {}
function JsContinue     () {}
function JsDefault      () {}
function JsDelete       () {}
function JsFalse        () {}
function JsFor          () {}
function JsFunction     () {}
function JsIf           () {}
function JsIn           () {}
function JsNew          () {}
function JsNull         () {}
function JsReturn       () {}
function JsSwitch       () {}
function JsThis         () {}
function JsThrow        () {}
function JsTrue         () {}
function JsTry          () {}
function JsTypeOf       () {}
function JsVar          () {}
function JsWhile        () {}

narcissus2object = function(tree, parent) {
    // Tree to objects

    sys.puts(tree.type);   
    if(tree.type == "18")
      sys.puts(tree.toString());
 
    var n = new {
        2: JsSemicolon,
        3: JsComma,
        4: JsAssign,
        5: JsHook,
        8: JsOr,
        9: JsAnd,
        11: JsBitwiseOr,
        12: JsBitwiseAnd,
        13: JsEq,
        14: JsNe,
        15: JsStrictEq,
        16: JsStrictNe,
        17: JsLt,
        18: JsLe,
        19: JsGe,
        20: JsGt,
        22: JsRightShift,
        24: JsPlus,
        25: JsMinus,
        26: JsMul,
        27: JsDiv,
        28: JsMod,
        29: JsNot,
        31: JsUnaryPlus,
        32: JsUnaryMinus,
        33: JsIncrement,
        34: JsDecrement,
        35: JsDot,
        42: JsScript,
        43: JsBlock,
        45: JsForIn,
        46: JsCall,
        47: JsNewWithArgs,
        48: JsIndex,
        49: JsArrayInit,
        50: JsObjectInit,
        51: JsPropertyInit,
        54: JsGroup,
        55: JsList,
        56: JsIdentifier,
        57: JsNumber,
        58: JsString,
        59: JsRegExp,
        60: JsBreak,
        61: JsCase,
        62: JsCatch,
        64: JsContinue,
        66: JsDefault,
        67: JsDelete,
        71: JsFalse,
        73: JsFor,
        74: JsFunction,
        75: JsIf,
        76: JsIn,
        78: JsNew,
        79: JsNull,
        80: JsReturn,
        81: JsSwitch,
        82: JsThis,
        83: JsThrow,
        84: JsTrue,
        85: JsTry,
        86: JsTypeOf,
        87: JsVar,
        89: JsWhile,
        }[tree.type]()
    n.init(tree, parent);
    return n;
    }


/* Parse the massive tree into objects, which are more code-readable */


function leftAndRightInit(t, parent) {
    this.parent = parent;
    this.left = narcissus2object(t[0], this);
    this.right = narcissus2object(t[1], this);
    }

function literalInit(t, parent) {
    this.parent = parent;
    this.value = t.value;
    }

function terminalInit(t, parent) {
    this.parent = parent;
    }

JsSemicolon.prototype.init = function(t, parent) {
    this.parent = parent;
    this.expression = narcissus2object(t.expression, this);
    }
JsComma.prototype.init = function(t, parent) {
    this.parent = parent;
    this.lines = listChildren(t, this);
    }
JsAssign.prototype.init = function(t, parent) {
    this.parent = parent;
    this.identifier = narcissus2object(t[0], this);
    this.operator = t.value == "=" ? ": " : " " + t.value + "= ";
    this.value = narcissus2object(t[1], this);
    }
JsHook.prototype.init = function(t, parent) {
    this.parent = parent;
    this.condition = narcissus2object(t[0], this);
    this.affirmative = narcissus2object(t[1], this);
    this.negative = narcissus2object(t[2], this);
    }
JsNot.prototype.init = function(t, parent) {
    this.parent = parent;
    this.condition = narcissus2object(t[0], this);
    }
function unaryOperatorInit(t, parent) {
    this.parent = parent;
    this.number = narcissus2object(t[0], this);
    }
JsUnaryPlus.prototype.init = unaryOperatorInit;
JsUnaryMinus.prototype.init = unaryOperatorInit;
JsDecrement.prototype.init = unaryOperatorInit;
JsIncrement.prototype.init = unaryOperatorInit;
JsDot.prototype.init = function(t, parent) {
    this.parent = parent;
    this.object = narcissus2object(t[0], this);
    this.property = narcissus2object(t[1], this);
    }
JsScript.prototype.init = function(t, parent) {
    this.parent = parent;
    this.lines = listChildren(t, this);
    /* MESSY! */
    /*for(var i = 0; i < this.lines.length; i++) {
        if (className(this.lines[i]) == "JsFunction"
            && this.lines[i].name == "function") {
            var func = this.lines[i];
            this.lines[i] = (function(name,func){
                this.parent = parent;
                this.identifier = (function(parent, value){
                    this.parent = parent;
                    this.value = value;
                    })(this, name);
                this.value = func;
                })(func.name, func);
            }
        }*/
    }
JsBlock.prototype.init = function(t, parent) {
    this.parent = parent;
    this.lines = listChildren(t, this);
    }
JsForIn.prototype.init = function(t, parent) {
    this.body = narcissus2object(t.body, this);
    this.iterator = narcissus2object(t.iterator, this);
    this.object = narcissus2object(t.object, this);
    }
JsCall.prototype.init = function(t, parent) {
    this.parent = parent;
    this.simple = typeof this.parent == JsSemicolon;
    this.callee = narcissus2object(t[0], this);
    this.args = listChildren(t[1], this);
    }
JsNew.prototype.init = function(t, parent) {
    this.parent = parent;
    this.identifier = narcissus2object(t[0], this);
    }
JsNewWithArgs.prototype.init = function(t, parent) {
    this.parent = parent;
    this.identifier = narcissus2object(t[0], this);
    this.list = narcissus2object(t[1], this);
    }
JsIndex.prototype.init = function(t, parent) {
    this.parent = parent;
    this.identifier = narcissus2object(t[0], this);
    this.index = narcissus2object(t[1], this);
    }
JsArrayInit.prototype.init = function(t, parent) {
    this.parent = parent;
    this.elements = listChildren(t, this);
    }
JsObjectInit.prototype.init = function(t, parent) {
    this.parent = parent;
    this.properties = listChildren(t, this);
    }
JsPropertyInit.prototype.init = function(t, parent) {
    this.parent = parent;
    this.key = narcissus2object(t[0], this);
    this.value = narcissus2object(t[1], this);
    }
JsGroup.prototype.init = function(t, parent) {
    this.parent = parent;
    this.inside = narcissus2object(t[0], this);
    }
JsList.prototype.init = function(t, parent) {
    this.parent = parent;
    this.elements = listChildren(t, this);
    }
JsIdentifier.prototype.init = function(t, parent) {
    this.parent = parent;
    this.simple = this.parent ? this.parent.simple : false;
    this.initializer = false;
    if (t.initializer) {
        this.initializer = narcissus2object(t.initializer, this);
        }
    this.value = t.value;
    }
JsFunction.prototype.init = function(t, parent) {
    this.parent = parent;
    this.params = t.params == "" ? [] : t.params.toString().split(",");
    this.body = narcissus2object(t.body, this);
    this.name = t.name;
    }
JsIf.prototype.init = function(t, parent) {
    this.parent = parent;
    this.condition = narcissus2object(t.condition, this);
    this.thenBlock = narcissus2object(t.thenPart, this);
    this.elseBlock = t.elsePart ? narcissus2object(t.elsePart, this) : null;
    }
JsReturn.prototype.init = function(t, parent) {
    this.parent = parent;
    this.noreturn = (t.value == "return");
    if (!this.noreturn) {
        this.value = narcissus2object(t.value, this);
        }
    }
JsTry.prototype.init = function(t, parent) {
    this.parent = parent;
    this.tryBlock = narcissus2object(t.tryBlock, this);
    this.catchClauses = [];
    for(var i = 0; i < t.catchClauses.length; i++) {
        this.catchClauses.push(narcissus2object(t.catchClauses[i], this));
        }
    this.finallyBlock = t.finallyBlock ? narcissus2object(t.finallyBlock, this) : null;
    }
JsTypeOf.prototype.init = function(t, parent) {
    this.parent = parent;
    this.expression = narcissus2object(t[0]);
    }
JsCatch.prototype.init = function(t, parent) {
    this.parent = parent;
    this.block = narcissus2object(t.block, this);
    this.varName = t.varName;
    }
JsCase.prototype.init = function(t, parent) {
    this.parent = parent;
    this.caseLabel = narcissus2object(t.caseLabel, this);
    this.block = narcissus2object(t.statements, this);
    this.varName = t.varName;
    }
JsFor.prototype.init = function(t, parent) {
    this.parent = parent;
    this.setup = t.setup ? narcissus2object(t.setup, this) : false;
    this.condition = narcissus2object(t.condition, this);
    this.update = t.update ? narcissus2object(t.update, this) : false;
    this.block = narcissus2object(t.body, this);
    }
JsVar.prototype.init = function(t, parent) {
    this.parent = parent;
    this.vars = listChildren(t, this);
    }
JsWhile.prototype.init = function(t, parent) {
    this.parent = parent;
    this.condition = narcissus2object(t.condition, this);
    this.block = narcissus2object(t.body, this);
    }

JsThrow.prototype.init = function(t, parent) {
    this.parent = parent;
    this.exception = narcissus2object(t.exception, this);
    }

JsDefault.prototype.init = function(t, parent) {
    this.parent = parent;
    this.block = narcissus2object(t.statements, this);
    }

JsDelete.prototype.init = function(t, parent) {
    this.parent = parent;
    this.index = narcissus2object(t[0], this);
    }

JsIn.prototype.init = function(t, parent) {
    this.parent = parent;
    this.property = narcissus2object(t[0], this);
    this.array = narcissus2object(t[1], this);
    }
JsSwitch.prototype.init = function(t, parent) {
    this.parent = parent;
    this.discriminant = narcissus2object(t.discriminant, this);
    this.cases = [];
    for(var i = 0; i < t.cases.length; i++) {
        this.cases.push(narcissus2object(t.cases[i], this));
        }
    }

JsOr.prototype.init = leftAndRightInit;
JsAnd.prototype.init = leftAndRightInit;
JsBitwiseOr.prototype.init = leftAndRightInit;
JsBitwiseAnd.prototype.init = leftAndRightInit;
JsEq.prototype.init = leftAndRightInit;
JsNe.prototype.init = leftAndRightInit;
JsStrictEq.prototype.init = leftAndRightInit;
JsStrictNe.prototype.init = leftAndRightInit;
JsLt.prototype.init = leftAndRightInit;
JsLe.prototype.init = leftAndRightInit;
JsGe.prototype.init = leftAndRightInit;
JsGt.prototype.init = leftAndRightInit;
JsRightShift.prototype.init = leftAndRightInit;
JsPlus.prototype.init = leftAndRightInit;
JsMinus.prototype.init = leftAndRightInit;
JsMul.prototype.init = leftAndRightInit;
JsDiv.prototype.init = leftAndRightInit;
JsMod.prototype.init = leftAndRightInit;

JsNumber.prototype.init = literalInit;
JsString.prototype.init = literalInit;
JsRegExp.prototype.init = literalInit;

JsNull.prototype.init = terminalInit;
JsThis.prototype.init = terminalInit;
JsTrue.prototype.init = terminalInit;
JsContinue.prototype.init = terminalInit;
JsFalse.prototype.init = terminalInit;
JsBreak.prototype.init = terminalInit;

/* pre-printing transforms */


function JsNodeToCs(n) {
    var nodeTransforms = {
        "JsScript": JsScriptToCs,
        "JsFunction": JsFunctionToCs,
        "JsBlock": JsBlockToCs,
        "JsArray": JsArrayToCs,
        "JsObject": JsObjectToCs,
        "JsFor": JsForToCs,
        "JsComma": JsCommaToCs,
        "JsVar": JsVarToCs,
        }
    c = className(n);
    if (c in nodeTransforms) {
        return nodeTransforms[c](n);
        }
    
    for (prop in n) {
      if (prop != "parent" && prop != 0) {
        n[prop] = JsNodeToCs(n[prop]);
        }
      }
    
    return n;
    }


function JsScriptToCs(n) {
    for (var i = 0; i < n.lines.length; i++) {
        n.lines[i] = JsNodeToCs(n.lines[i]);
        }
    return n;
    }

function JsFunctionToCs(n) {
    if (n.name != "function" && (className(n.parent) == "JsScript" || className(n.parent) == "JsBlock")) { // function declaration
        var ass = new JsAssign();
        ass.parent = n.parent;
        var id = new JsIdentifier();
        id.value = n.name;
        id.parent = ass;
        ass.identifier = id;
        ass.value = n;
        ass.operator = ": ";
        n.name = "function";
        n.body = JsNodeToCs(n.body);
        return ass;
        }
    return n;
    }

function JsBlockToCs(n) {
    for (var i = 0; i < n.lines.length; i++) {
        n.lines[i] = JsNodeToCs(n.lines[i]);
        }
    return n;
    }

function JsArrayToCs(n) {
    for (var i = 0; i < n.elements.length; i++) {
        n.elements[i] = JsNodeToCs(n.elements[i]);
        }
    return n;
    }

function JsObjectToCs(n) {
    for (var i = 0; i < n.properties.length; i++) {
        n.properties[i] = JsNodeToCs(n.properties[i]);
        }
    return n;
    }


function JsVarToCs(n) {
    // Convert multiple-var declarations to multiple var-declarations
    if(n.vars.length > 1) {
        var b = new JsBlock();
        b.parent = n.parent;
        b.lines = [];
        for(var i = 0; i < n.vars.length; i++) {
            var s = new JsSemicolon();
            s.parent = b;
            var v = new JsVar();
            s.expression = v;
            v.parent = s;
            v.vars = [JsNodeToCs(n.vars[i])];
            b.lines.push(s);
            }
        return b;
        }
    return n;
    }

function JsForToCs(n) {
    /* No for loop -- convert to a while loop.  Returns a block.
     */
    
    n.setup = JsNodeToCs(n.setup);
    n.condition = JsNodeToCs(n.condition);
    n.update = JsNodeToCs(n.update);
    n.block = JsNodeToCs(n.block);
    
    var b = new JsBlock();
    b.parent = n.parent;
    b.lines = [];
    
    if ( n.setup ) b.lines.push(n.setup);
    
    var w = new JsWhile();
    
    w.parent = n.parent;
    w.condition = n.condition;
    w.block = n.block;
    
    if(!(w.block.lines)) w.block.lines = [];
    
    if (n.update) w.block.lines.push(n.update);
    
    b.lines.push(w);
    
    return b;
    }

function JsCommaToCs(n) {
    var b = new JsBlock();
    b.parent = n.parent;
    b.lines = [];
    for(var i = 0; i < n.lines.length; i++) {
        var s = new JsSemicolon();
        s.parent = b;
        s.expression = n.lines[i];
        b.lines.push(s);
        }
    return b;
    }



function normalizeBlocks(n) {
    // If there are blocks in blocks, flatten them
    
    c = className(n);
    
    if(c == "JsBlock" || c == "JsScript") {
        var newLines = [];
        for(var i = 0; i < n.lines.length; i++) {
            var l = n.lines[i];
            if( className(l) == "JsBlock") {
                for(var j = 0; j < l.lines.length; j++) {
                    normalizeBlocks(l.lines[j]);
                    newLines.push(l.lines[j]);
                    }
                }
            else {
                normalizeBlocks(l);
                newLines.push(l);
                }
            }
        n.lines = newLines;
        }
    else {
        for (prop in n) {
          if (prop != "parent" && prop != 0) {
            normalizeBlocks(n[prop]);
            }
          }
        }
    }


/* Helper printing functions */

function indent(s) {
    return "\n  " + s.trim().replace(/\n/g, "\n  ") + "\n";
    }

function coffeeList(list) {
    var s = [];
    for(var i = 0; i < list.length; i++) {
        s.push(list[i].coffee());
        }
    return s;
    }

function escapeString(s) {
    if (s.indexOf('"') == -1) {
        // If there are no double quotes,
        // use them
        return '"' + s + '"';
        }
    else if (s.indexOf("'") == -1) {
        // Otherwise, try single quotes 
        return "'" + s + "'";
        }
    // And if even that's awkward, escape the single chars.
    return "'" + s.replace(/'/g, "\\'") + "'";
    }


/* Printing the parse tree as CoffeeScript */

function getleftAndRightCoffeeFunction(op) {
    return function() {
        return this.left.coffee() + " " + op + " " + this.right.coffee();
        }
    }


JsPlus.prototype.coffee = function() {
    /* Deal with string interpolation.
     * Reason as follows:
     * String + Anything = String.
     * If either left or right is a string, we know we're returning a string.
     * Therefore we can interpolate them: "${left}${right}".
     * If one is a string literal, we can include it directly:
     *   if left is a string literal "foo", then "foo${right}".
     *   (if both string literals, "foo" and "bar", then "foobar"!)
     * If an identifier, we don't need braces: "foo$right".
     * 
     * Problem: how do we know if a node is a string?
     * Solution:
     *   a call evaluatesToString(),
     *   returning true on string literals,
     *   calling itself recursively on JsPluses,
     *   and returning false for anything else.
     */
    
    function evaluatesToString(n) {
        var cl = className(n);
        if (cl == "JsString") return true;
        if (cl == "JsPlus") return (evaluatesToString(cl.left) && evaluatesToString(cl.right));
        return false;
        }
    
    function toInterpolatedValue(n) {
        if(className(n) == "JsString") {
            return n.value;
            }
        else if(className(n) == "JsIdentifier") {
            return "$" + n.coffee();
            }
        else if(className(n) == "JsGroup") {
            return toInterpolatedValue(n.inside);
            }
        else if(className(n) == "JsPlus") {
            return toInterpolatedValue(n.left) + toInterpolatedValue(n.right);
            }
        else {
            return "${" + n.coffee() + "}";
            }
        }
    
    if( evaluatesToString(this.left) || evaluatesToString(this.right)) {
        return escapeString( toInterpolatedValue(this) );
        }
    
    return this.left.coffee() + " + " + this.right.coffee();
    }
JsRightShift.prototype.coffee = getleftAndRightCoffeeFunction(">>");
JsMinus.prototype.coffee = getleftAndRightCoffeeFunction("-");
JsMul.prototype.coffee = getleftAndRightCoffeeFunction("*")
JsDiv.prototype.coffee = getleftAndRightCoffeeFunction("/");
JsNot.prototype.coffee = function() {
    return "not " + this.condition.coffee();
    }
JsUnaryPlus.prototype.coffee = function() { return this.number.coffee(); } // Is an explicit plus ever needed ... ?
JsUnaryMinus.prototype.coffee = function() {
    return "-" + this.number.coffee();
    }
JsIncrement.prototype.coffee = function() {
    return this.number.coffee() + "++";
    }
JsDecrement.prototype.coffee = function() {
    return this.number.coffee() + "--";
    }
JsDot.prototype.coffee = function() {
    if(     className(this.object) == "JsThis"
        &&  className(this.property) != "JsFunction") {
        return "@" + this.property.coffee();
        }
    else if(this.property.value == "prototype" && className(this.parent) == "JsDot") {
        return this.object.coffee() + "::";
        }
    var l = this.object.coffee();
    return l +
           (l.match(/\:\:$/) ? "" : "." ) + // messy ...
           this.property.coffee();
    }
JsScript.prototype.coffee = function() {
    return coffeeList(this.lines).join('');
    }
JsSemicolon.prototype.coffee = function() {
    return this.expression.coffee() + "\n";
    }
JsComma.prototype.coffee = function() {
    return coffeeList(this.lines).join('');
    }
JsAssign.prototype.coffee = function() {
    return this.identifier.coffee() + this.operator + (className(this.value) == "JsGroup" ? this.value.inside.coffee() : this.value.coffee());
    }
JsHook.prototype.coffee = function() {
    return "if " + this.condition.coffee() + " then " + this.affirmative.coffee() + (className(this.parent) == "JsSemicolon" ? "" : " else " + this.negative.coffee());
    }
JsOr.prototype.coffee = function() {
    var bits = [];
    if(!(    className(this.parent) == "JsAssign"
        && className(this.parent.identifier) == "JsIdentifier"
        && className(this.left) == "JsIdentifier"
        && this.left.value == this.parent.identifier.value)) {
        bits.push(this.left.coffee());
        }
    bits.push("or");
    bits.push(this.right.coffee());
    return bits.join(' ');
    }
JsAnd.prototype.coffee = getleftAndRightCoffeeFunction("and");
JsBitwiseOr.prototype.coffee = getleftAndRightCoffeeFunction("|");
JsBitwiseAnd.prototype.coffee = getleftAndRightCoffeeFunction("&");
JsEq.prototype.coffee = getleftAndRightCoffeeFunction("==");
JsNe.prototype.coffee = getleftAndRightCoffeeFunction("!=");
JsStrictEq.prototype.coffee = getleftAndRightCoffeeFunction("is");
JsStrictNe.prototype.coffee = getleftAndRightCoffeeFunction("isnt");
JsLt.prototype.coffee = getleftAndRightCoffeeFunction("<");
JsLe.prototype.coffee = getleftAndRightCoffeeFunction("<=");
JsGe.prototype.coffee = getleftAndRightCoffeeFunction(">=");
JsGt.prototype.coffee = getleftAndRightCoffeeFunction(">");
JsMod.prototype.coffee = getleftAndRightCoffeeFunction("%");
JsBlock.prototype.coffee = function() {
    return coffeeList(this.lines).join('');
    }
JsForIn.prototype.coffee = function() {
    return "for " + this.iterator.coffee() + " in " + this.object.coffee() + indent(this.body.coffee());
    }
JsCall.prototype.coffee = function() {
    var s = this.callee.coffee();
    var args = coffeeList(this.args).join(", ");
    if (className(this.parent) == "JsSemicolon" && args.length > 0) {
        args = " " + args;
        }
    else {
        args = "(" + args + ")";
        }
    return s + args;
    }
JsNewWithArgs.prototype.coffee = function() {
    return "new " + this.identifier.coffee() + this.list.coffee();
    }
JsNew.prototype.coffee = function() {
    return "new " + this.identifier.coffee() + "()";
    }
JsIndex.prototype.coffee = function() {
    return this.identifier.coffee() + "[" + this.index.coffee() + "]";
    }
JsArrayInit.prototype.coffee = function() { 
    return "[" + coffeeList(this.elements).join(", ") + "]";
    }
JsObjectInit.prototype.coffee = function() {
    var multiple_line_heuristic = this.properties.length > 0
    if(multiple_line_heuristic) {
        return "{" + indent(coffeeList(this.properties).join("\n") + "\n}");
        }
    else {
        return "{" + coffeeList(this.properties).join(", ") + "}";
        }
    }
JsPropertyInit.prototype.coffee = function() {
    return this.key.coffee() + ": " + this.value.coffee();
    }
JsGroup.prototype.coffee = function() {
    // see if this is equivalent to the existential operator.
    // this is a bit more complex than expected.
    if (    className(this.inside) == "JsAnd"               &&
            className(this.inside.left) == "JsStrictNe"     &&
            className(this.inside.right) == "JsStrictNe"
            ) {
        var left = this.inside.left;
        var right = this.inside.right;

        if (className(left.left) in [null, "undefined"]) {
            // as !== is commutative, enforce a standard order.
            var tmp = left.left;
            left.left = left.right;
            left.right = tmp;
            }
        
        if (className(right.left) in [null, "undefined"]) {
            // as !== is commutative, enforce a standard order.
            var tmp = right.left;
            right.left = right.right;
            right.right = tmp;
            }
        
        if (className(left.left) == "JsIdentifier") {
            // as && is commutative, enforce a standard order.
            var tmp = left;
            left = right;
            right = tmp;
            }
        
        if(
            className(left.left) == "JsTypeOf" &&
            className(left.right) == "JsString" &&
            left.right.value == "undefined" &&
            className(right.left) == "JsIdentifier" &&
            className(right.right) == "JsNull" &&
            left.left.expression.value == right.left.value
            ) {
            return left.left.expression.value + "?";
            }
        
        }
    // otherwise, return as normal
    return "(" + this.inside.coffee() + ")";
    }
JsList.prototype.coffee = function() {
    return "(" + coffeeList(this.elements).join(", ") + ")";
    }
JsIdentifier.prototype.coffee = function() {
    s = this.value;
    if (className(this.parent) == "JsVar") {
        s += ": ";
        s += this.initializer ? this.initializer.coffee() : "null";
        }
    
    return s;
    }
JsNumber.prototype.coffee = function() {
    return this.value.toString();
    }
JsString.prototype.coffee = function() {
    return escapeString(this.value.toString());
    }
JsRegExp.prototype.coffee = function() {
    return this.value.toString();
    }
JsFunction.prototype.coffee = function() {  
    var s = "";
    
    if (this.params.length > 0) {
        s += "(" + this.params.join(", ") + ") ";
        }
    
    s += "-> ";
    
    s += this.body.lines.length > 1 ? indent(this.body.coffee()) : this.body.coffee();
    
    return s;
    }
JsIf.prototype.coffee = function() {
    if (this.elseBlock) {
        // There is an "else" block
        var c = this.condition;
        if( className(c) == "JsGroup") {
            c = c.inside;
            }
        
        return     this.thenBlock.lines
                && this.thenBlock.lines.length == 1
                && className(this.elseBlock) == "JsBlock"
                && this.elseBlock.lines.length == 1
                
                /* suitable for */  ? this.thenBlock.coffee().trim()    +
                /* a single line */   " if "                            +
                                      c.coffee()                        +
                                      " else "                          +
                                      this.elseBlock.coffee().trim()    +
                                      "\n"
                                                 
                /* multiple-line */ : "if "                             +
                /* syntax form */     c.coffee()                        +
                                      indent(this.thenBlock.coffee())   +
                                      "else "                           +
                                      indent(this.elseBlock.coffee())
              ;
        }
    else {
        // There is no `else` block
        var kw = "if";
        var c = this.condition;
        if (className(c) == "JsNot") {
            kw = "unless";
            c = c.condition;
            }
        if( className(c) == "JsGroup") {
            c = c.inside;
            }
        
        return  className(this.thenBlock) != "JsBlock" || this.thenBlock.lines.length == 1 ?  // If suitable for single line
                this.thenBlock.coffee().trim() + " " + kw + " " + c.coffee() + "\n" :         // Single-line syntax
                kw + " " + c.coffee() + indent(this.thenBlock.coffee());                      // Multiple-line
        }
    }

JsThis.prototype.coffee = function() {
    return "this";
    }
JsThrow.prototype.coffee = function() { return "throw " + this.exception.coffee(); }
JsTrue.prototype.coffee = function() {
    return "true";
    }
JsTry.prototype.coffee = function() {
    var s = "try" + indent(this.tryBlock.coffee());
    for(var i = 0; i < this.catchClauses.length; i++) {
        s += "catch" + indent(this.catchClauses[i].coffee());
        }
    if (this.finallyBlock) {
        s += "finally" + indent(this.finallyBlock.coffee());
        }
    return s;
    }
JsTypeOf.prototype.coffee = function() {
    return "typeof " + this.expression.coffee();
    }
JsFalse.prototype.coffee = function() {
    return "false";
    }
JsCatch.prototype.coffee = function() {
    return coffeeList(this.block.lines).join('');
    }
JsFor.prototype.coffee = function() {
    return "[FORLOOP]";
    }
JsNull.prototype.coffee = function() {
    return "null";
    }
JsContinue.prototype.coffee = function() {
    return "continue";
    }
JsBreak.prototype.coffee = function() {
    return "break";
    }
JsReturn.prototype.coffee = function() {
    if(this.noreturn) {
        return "return\n";
        }
    
    var s = "";
    
    if(className(this.parent) == "JsBlock" && this != this.parent.lines[this.parent.lines.length-1]) {
        // If we're not last, we have to explicitly return
        s += "return ";
        }
    
    s += this.value.coffee();
    
    return s + "\n";
    }
JsVar.prototype.coffee = function() {
    var s = [];
    for(var i=0; i < this.vars.length; i++) {
        var v = this.vars[i];
        // Do we ever need to declare an uninitialized new variable in CS?
        if(v.initializer) { s.push(v.coffee() + "\n"); }
        }
    return s.join("");
    }

JsWhile.prototype.coffee = function() {
    var kw = "while";
    var c = this.condition;
    if (className(c) == "JsNot") {
        kw = "until";
        c = c.condition;
        }
    if( className(c) == "JsGroup") {
        c = c.inside;
        }
    
    return this.block.lines && this.block.lines.length == 1 ? this.block.coffee().trim() + " " + kw + " " + c.coffee() + "\n" : kw + " " + c.coffee() + indent(this.block.coffee());
    }

JsDelete.prototype.coffee = function() {
    return "delete " + this.index.coffee();
    }

JsIn.prototype.coffee = function() {
    return this.property.coffee() + " in " + this.array.coffee();
    }

JsSwitch.prototype.coffee = function() {
    var s = "switch " + this.discriminant.coffee() + "\n"
    for(var i = 0; i < this.cases.length; i++) {
        s += this.cases[i].coffee();
        }
    return s;
    }

JsCase.prototype.coffee = function() {
        
    var s = "when " + this.caseLabel.coffee()
    if (this.block.lines.length > 1) {
        return s + indent(this.block.coffee());
        }
    else {
        return s + " then " + this.block.coffee();
        }
    }

JsDefault.prototype.coffee = function() {
    if (this.block.lines.length > 1) {
        return "else" + indent(this.block.coffee());
        }
    else {
        return "else " + this.block.coffee();
        }
    }


/*
OptionParser = require("./optparse").OptionParser;

parser = new OptionParser([["-i", "--in-file", "In file"], ["-o", "--out-file", "Out file"]]);
options = parser.parse(process.argv);

for(p in options["in-file"]) sys.puts(p);
*/

infile = "orm.migrations.js";
outfile = "orm.migrations.coffee";

sys.puts(infile);
sys.puts(outfile);

if (infile && outfile) {
  fs.readFile(infile, function(err, file) {
    jstree = jsparse(file, infile, 0);
    objects = narcissus2object(jstree);
    objects = JsNodeToCs(objects);
    normalizeBlocks(objects);
    fs.writeFile(outfile, objects.coffee(), function(err){
      if(err) throw err;
      sys.puts("Saved to " + outfile);
      });
    });
  }
else {
  sys.puts("Please provide -i and -o.");
  }

