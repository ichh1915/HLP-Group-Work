// Save Monaco's amd require and restore Node's require
var amdRequire = global.require;
global.require = nodeRequire;

// require node modules before loader.js comes in
var path = require('path');
function uriFromPath(_path) {
    var pathName = path.resolve(_path).replace(/\\/g, '/');
    if (pathName.length > 0 && pathName.charAt(0) !== '/') {
        pathName = '/' + pathName;
    }
    return encodeURI('file://' + pathName);
}


const extensions = ['', 'EQ', 'NE', 'MI', 'PL', 'HI', 'HS', 'LO', 'LS', 'GE', 'GT', 'LE', 'LT', 'VS', 'VC', 'NV', 'AL'];

const prefix = [
    'MOV', 'MOVS', 'MVN', 'MVNS', 'ADD', 'ADDS', 'ADC', 'ADCS', 'SUB', 'SUBS', 'SBC', 'SBCS',
    'RSB', 'RSBS', 'RSC', 'RSCS', 'CMP', 'CMN',
    'B',
    'ORR', 'AND', 'EOR', 'BIC', 'TST', 'TEQ', 'LDR',
    'STR', 'ADR', 'LDM', 'STM', 'ADR'
    ];

const misc = [
    'EQU', 'FILL', 'DCD', 'DCB', 'END'
    ];

var keywords = [];

var regs = [
    'R0', 'R1', 'R2', 'R3', 'R4', 'R5', 'R6', 'R7', 'R8', 'R9', 'R10',
    'R11', 'R12', 'R13', 'R14', 'R15', 'PC', 'SP', 'LR'
    ];

var shifts = ['LSL', 'LSLS', 'LSR', 'LSRS', 'ASR', 'ROR', 'RRX'];

prefix.forEach(function (p) {
   extensions.forEach(function (e) {
       keywords.push(p + e);
   });
});

amdRequire.config({
    baseUrl: uriFromPath(path.join(__dirname, '../node_modules/monaco-editor/min'))
});

// workaround monaco-css not understanding the environment
self.module = undefined;

// workaround monaco-typescript not understanding the environment
self.process.browser = true;

amdRequire(['vs/editor/editor.main'], function () {

    monaco.languages.register({
        id: 'arm'
    });

    monaco.languages.setMonarchTokensProvider('arm', {
        // Set defaultToken to invalid to see what you do not tokenize yet
        defaultToken: 'invalid',

        ignoreCase: true,

        brackets: [
            ['{', '}', 'delimiter.curly'],
            ['[', ']', 'delimiter.square'],
            ['(', ')', 'delimiter.parenthesis'],
            ['<', '>', 'delimiter.angle']
        ],

        exp: ['#'],

        operators: [
            '=', '>', '!', '+', '-', '*'
        ],

        // TODO:- Complete keyword map
        shifts: shifts,

        keywords: keywords,

        registers: regs,

        misc: misc,  
        
        // we include these common regular expressions
        symbols: /[=><!~?:&|+\-*\/\^%]+/,

        // C# style strings
        escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

        // The main tokenizer for our languages
        tokenizer: {
            root: [
                // identifiers and keywords
                [/[a-z_$][\w$]*/, {
                    cases: {
                        '@keywords': 'keyword',
                        '@misc': 'misc',
                        '@shifts': 'shift',
                        '@registers': 'register',
                        '@default': 'identifier'
                    }
                }],

                //  expression  
                [/[#]/, 'hash'],

                // whitespace
                { include: '@whitespace' },

                // delimiters and operators
                [/[{}()\[\]]/, '@brackets'],
                [/[<>](?!@symbols)/, '@brackets'],
                [/@symbols/, {
                    cases: {
                        '@operators': 'operator',
                        '@default': ''
                    }
                }],

                // @ annotations.
                // As an example, we emit a debugging log message on these tokens.
                // Note: message are supressed during the first load -- change some lines to see them.
                [/@\s*[a-zA-Z_\$][\w\$]*/, { token: 'annotation', log: 'annotation token: $0' }],

                // numbers
                [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
                [/0[xX][0-9a-fA-F]+/, 'number.hex'],
                [/0[bB][0-1]+/, 'number.bin'],
                [/\d+/, 'number'],

                // delimiter: after number because of .\d floats
                [/[;,.]/, 'delimiter'],

                // strings
                [/"([^"\\]|\\.)*$/, 'string.invalid'],  // non-teminated string
                [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

                // characters
                [/'[^\\']'/, 'string'],
                [/(')(@escapes)(')/, ['string', 'string.escape', 'string']],
                [/'/, 'string.invalid']

            ],

            comment: [
                [/[^\/*]+/, 'comment'],
                [/\/\*/, 'comment', '@push'],    // nested comment
                ["\\*/", 'comment', '@pop'],
                [/[\/*]/, 'comment']
            ],

            string: [
                [/[^\\"]+/, 'string'],
                [/@escapes/, 'string.escape'],
                [/\\./, 'string.escape.invalid'],
                [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
            ],

            whitespace: [
                [/[ \t\r\n]+/, 'white'],
                [/;.*$/, 'comment']
            ]
        }
    });

    monaco.editor.defineTheme('myCoolTheme', {
        base: 'vs-dark',
        inherit: true,
        rules: [
            { token: 'keyword', foreground: '32a0d1', fontStyle: 'bold'},
            { token: 'misc', foreground: '32a0d1', fontStyle: 'bold'},
            { token: 'shift', foreground: 'b5e5da', fontStyle: 'bold'},
            { token: 'identifier', foreground: '808080'},
            { token: 'operator', foreground: 'ffd662'},
            { token: 'comment', foreground: '808080'},
            { token: 'number', foreground: 'f7882f'},
            { token: 'number.hex', foreground: 'f7c331'},
            { token: 'number.float', foreground: 'dcc7aa'},
            { token: 'number.bin', foreground: 'd5a149'}, 
            { token: 'hash', foreground: 'dcc7aa'},
            { token: 'register', foreground: '20b2aa'}
        ]
    });

    //TODO:- Add all autocomplete
    function createRegProposals() {
        var instrs = [];
        
        regs.forEach(function (r) {
           var instr = {
               label: r,
               insertText: r
           };
           
           instrs.push(instr);
        });

        shifts.forEach(function (s) {
            var instr = {
                label: s,
                insertText: s
            };

            instrs.push(instr)
        });

        misc.forEach(function (m) {
            var instr = {
                label: m,
                insertText: m + ' '
            };

            instrs.push(instr);
        });
        
        return instrs;
    }
    

    
    function createInstructionProposals() {
        var instrs = [];
        
        keywords.forEach(function (l) {
            var instr = {
                label: l,
                insertText: l + ' '
            };
            instrs.push(instr)
        });
        
        return instrs;
    }

    monaco.languages.registerCompletionItemProvider('arm', {
        provideCompletionItems: function(model, position) {
            
            var textUntilPosition = model.getValueInRange({startLineNumber: position.lineNumber, startColumn: 1, endLineNumber: position.lineNumber, endColumn: position.column});
            
            var regexFromMyArray = new RegExp(keywords.join("|"), 'gi'); //g = global match, i = case-insensitive
            var regexFromReg = new RegExp(regs.join("|", 'gi'));
            
            var match = textUntilPosition.match(
                /./g
            );
            
            var alreadyMatch = textUntilPosition.match(
              regexFromMyArray  
            );
            
            if (match && !alreadyMatch) {
                return createInstructionProposals();
            } else if (alreadyMatch) {
                return createRegProposals();
            }
            return [];
        }
    });

    window.code = monaco.editor.create(document.getElementById('editor'), {
        language: 'arm',
        theme: 'myCoolTheme',
        renderWhitespace: 'all',
        roundedSelection: false,
        scrollBeyondLastLine: false
    });

});
