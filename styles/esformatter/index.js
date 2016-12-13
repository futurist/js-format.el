const {format} = require('esformatter')
const style = require('./style.json')

/**
 * format js code, then response back to user
 * @param {string} code the string of code to format
 * @param {function} cb callback to call when format finished: (err, out)->any
 * @returns {any}
 */
function formatter (code, cb) {
  try {
    let result = format(code, style)
    cb(null, result)
  } catch (e) {
    cb(e)
  }
}

module.exports = formatter
