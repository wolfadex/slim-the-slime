const fs = require("fs");
const esbuild = require("esbuild");

const pureFuncs = [ "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"];

fs.readFile("dist/elm.js", "utf8", function(err, elmCode) {
    // Remove IIFE.
    const newCode =
        "var scope = window;" +
        elmCode.slice(elmCode.indexOf("{") + 1, elmCode.lastIndexOf("}"));

    const result = esbuild.transformSync(newCode, {
    minify: true,
    pure: pureFuncs,
    target: "es5",
    format: "iife",
    });
    fs.writeFile("dist/elm.js", result.code, function(err){
        // 
    });
});

