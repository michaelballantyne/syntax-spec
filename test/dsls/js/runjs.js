const util = require('util');
const escodegen = require('escodegen')
const vm = require("vm")

const read_stdin = util.promisify(function(callback) {
    var inputChunks = [];

    process.stdin.resume();
    process.stdin.setEncoding('utf8');

    process.stdin.on('data', function (chunk) {
        inputChunks.push(chunk);
    });

    process.stdin.on('end', function () {
        callback(undefined, inputChunks.join());
    });
});

function eval_module(text) {
    return vm.runInNewContext(text, {setImmediate: setImmediate, console: console, require: require, process: process});
}


read_stdin()
  .then(function(res) {
    const program = escodegen.generate(JSON.parse(res));
    process.stderr.write(program + "\n")
    console.log(eval_module(program))
  })


