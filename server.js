// #!/usr/bin/env node

const http = require('http')
const path = require('path')
const url = require('url')
const exec = require('child_process').exec
const styles = require('./styles.json')

const errorsign = '#!!#'
const port = 58000
const timeout = 10e3  // 10 sec

const server = http.createServer((req, res) => {
  let bodyString = ''
	const timeoutFn = function (socket) {
		console.log('server', 'request timeout')
		res.end(errorsign + 'js-format server request timeout, try again?')
	}
  req.resume()
	// prevent node died to set a timeout
	res.setTimeout(timeout, timeoutFn)
  res.writeHead(200, {'Content-Type': 'text/plain'})
	if(req.url=='/') {
		return res.end('js-format server')
	}
	// segments length should > 1
	const segments = req.url.split('/')
	const query = url.parse(req.url, true).query
	const style = segments[2]
	let styleObj = {}
	let styleFolder
	let styleEntry

	if(style) {
		styleObj = styles[style]
		if(!styleObj || typeof styleObj!=='object') {
			return res.end(errorsign + 'there\'s no style configed: "'+style+'"')
		}
		// style base folder
		styleFolder = styleObj.folder || style
		styleEntry = styleObj.entry || ''
	}

  if (req.method == 'POST' && segments[1]=='format' && styleFolder) {
		let format
		try {
			format = styleObj.formatter = require('./' + styleFolder + '/' + styleEntry )
		} catch(err) {
			return res.end(errorsign + 'cannot require formatter for '+style+', maybe it\'s in setup process? If not, please run `js-format-setup\'')
		}
    req.on('data', function (data) {
			bodyString += data
		})

    req.on('end',  () => {
      let code = bodyString || ''
      // console.log(bodyString)
      code = new Buffer(code, 'base64').toString()
			format(code, (err, result) => {
				console.log(err, result)
				if(res.finished) return console.log('timeout, but result is', result)
				if (err) res.end(errorsign + JSON.stringify(err))
				else res.end(result)
			})
    })
  }
  if (req.method == 'GET') {
    console.log('GET', req.url, styleFolder, segments)
		if(segments[1]=='setup' && styleFolder) {
			console.log('setting up', style)
			try {
				styleObj.formatter = require('./' + styleFolder + '/' + styleEntry)
				res.end('completed with style: '+ style)
			} catch(err) {
				// check err type
				if(err.code !== 'MODULE_NOT_FOUND') return res.end(JSON.stringify(err))
				// error is module_not_found, run npm install
				const command = query.command || 'cnpm install'
				console.log('setup', style, 'using', command)
				// 1 min install
				res.setTimeout(query.timeout || 60e3, timeoutFn)
				const child = exec(command, {cwd: path.join(__dirname, styleFolder)}, function(err, stdout, stderr) {
					console.log(err, stdout, stderr)
					// now should can safely require
					styleObj.formatter = require('./' + styleFolder + '/' + styleEntry)
					// response timeout already?
					if(res.finished) return
					if(err) {
						return res.end(command + ' error \n' + JSON.stringify(err))
					}
					res.end(command + ' result:\n[stdout]:\n'+stdout+'\n[stderr]:\n'+stderr)
				})
			}
			return
		}
		switch (segments[1]) {
		case 'exit':
			console.log('exit now')
			res.end('')
			process.exit(0)
			break
		case 'getport':
			res.end(port + '')
			break
		default:
			res.end('undefined action', req.url)
		}
  }
}).listen(port, function () {
  console.log('Listening on port', port)
})
