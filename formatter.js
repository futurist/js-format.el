const loc = require('src-location')
const standard = require('standard')

/**
 * format js code using standard, then response back to user
 * @param {string} code the string of code to format
 * @param {function} cb callback to call when format finished: (err, out)->any
 * @returns {any}
 */
function format(code, cb) {
	standard.lintText(code, {fix: true}, (err, out) => {
		console.log(err, code)
		if(err) return cb(err)
		const ret = out.results[0]
		const error = ret.messages.filter(v => v.fatal).shift()
		if (error) {
			// convert line+column into zero-based index
			// error.index is important for emacs to locate the error pos
			error.index = loc.locationToIndex(code, error.line, error.column) - 1
			console.log(JSON.stringify(error))
		}
		cb(error, ret.output)
	})
}

module.exports = format
