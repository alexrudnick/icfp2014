#!/usr/bin/env phantomjs

var system = require('system');
var webpage = require('webpage');
var fs = require('fs');
var child_process = require('child_process');
var page = null;

String.prototype.endsWith = function(suffix) {
    return this.indexOf(suffix, this.length - suffix.length) !== -1;
};

function runStep() {
    var done = page.evaluate(function() {
        window.state = stepProg(window.program, window.state.state, false);
        if (state.stop_reason == "TraceTrace") {
            console.log(state.traceval);
            return false;
        }

        if (state.stop_reason == "TraceFault") {
            console.log(state.stop_reason + ": " + state.faultcode);
            return true;
        }

        if (state.stop_reason == "TraceStop") {
           $("#state").html(renderState(window.program, window.state.state));
            console.log('Next instruction: ' +
                $(".instr")[0].children[1].innerText + ' ' +
                $(".instr")[0].children[2].innerText);

            console.log('Data stack and control stack:');
            var stack = $('.dataframe');
            for (var i = 0; i < stack.length; i++) {
                console.log('   ' + stack[i].innerHTML);
            }

            return true;
        }

        console.log("Unknown state");
        return true;
    });

    if (done)
        phantom.exit();

    setTimeout(runStep, 0);
}

function lambdaManSimulatorURL() {
    return "file:///" + scriptDirectory() + "/../simulator/lambdaman.html";
}

function runProgram(sourceCode) {
    page = webpage.create();
    page.onConsoleMessage = function (text) { console.log(text); };
    page.open(lambdaManSimulatorURL(), function() {
        var parse = page.evaluate(function(programText) {
            var parseResult = parseProg(programText);

            if (parseResult.error != null) {
                console.log("Parse Error: " + parseResult.error);
                return false;
            }

            window.program = parseResult.prog;
            window.state = {state: initState()};
            return true;
        }, sourceCode);

        if (!parse)
            phantom.exit();

        setTimeout(runStep, 0);
    });
}

function scriptDirectory() {
    var relativeScriptPath = system.args[0];
    var absoluteScriptPath = fs.absolute(relativeScriptPath);
    return absoluteScriptPath.substring(0, absoluteScriptPath.lastIndexOf('/'));
}

function loadAndRunLispProgram(lispFilename) {
    child_process.execFile(scriptDirectory() + "/compiler.py", [lispFilename], null,
        function (err, stdout, stderr) {
            console.log(stderr);
            if (err != 0)
                phantom.exit();

            runProgram(stdout);
        });
}

function loadAndRunProgram() {
    if (system.args.length <= 1) {
        console.error("Missing path to lambdaman program.\n");
        phantom.exit();
    }
    var sourceFilename = system.args[1];
    if (sourceFilename.endsWith(".lisp")) {
        loadAndRunLispProgram(sourceFilename);
        return;
    }

    runProgram(fs.read(sourceFilename));
}

loadAndRunProgram();

