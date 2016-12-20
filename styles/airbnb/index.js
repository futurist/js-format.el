// this file formatted using this lib

var babel = require('babel-core');

/**
 * format js code, then response back to user
 * @param {string} code the string of code to format
 * @param {function} cb callback to call when format finished: (err, out)->any
 * @returns {any}
 */
function format(code, cb) {
  try {
    var result = babel.transform(code, {
      presets: ["airbnb"]
    });
    cb(null, result.code);
  } catch (e) {
    e.index = e.pos;
    cb(e);
  }
}

module.exports = format;
