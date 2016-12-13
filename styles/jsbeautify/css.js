const { css_beautify: beautify } = require('js-beautify')
const style = require('./style')

/**
 * format js code, then response back to user
 * @param {string} code the string of code to format
 * @param {function} cb callback to call when format finished: (err, out)->any
 * @returns {any}
 */
function format (code, cb) {
  cb(null, beautify(code, style))
}

module.exports = format
