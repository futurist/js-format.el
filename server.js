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
    const msg = ''
    if (styleObj && styleObj.setupProc) {
      msg = ' Bug the setup proc still working, `js-format-exit\' to terminate.'
    }
    res.end(errorsign + 'js-format server request timeout.' + msg)
  }
  req.resume()
	// prevent node died to set a timeout
  res.setTimeout(timeout, timeoutFn)
  res.writeHead(200, {'Content-Type': 'text/plain'})
  if (req.url == '/') {
    return res.end('js-format server')
  }
	// segments length should > 1
  const segments = req.url.split('/')
  const query = url.parse(req.url, true).query
  const style = segments[2]
  let styleObj = {}
  let styleFolder
  let styleEntry
	let stylePkg
	let styleSetup

  if (style) {
    styleObj = styles[style]
    if (!styleObj || typeof styleObj !== 'object') {
      return res.end(errorsign + 'there\'s no style configed: "' + style + '"')
    }
		// style base folder
    styleFolder = styleObj.folder || style
    styleEntry = './' + styleFolder + '/' + (styleObj.entry || '')
		stylePkg = require('./' + styleFolder + '/package.json')
		styleSetup = stylePkg.setup && typeof stylePkg.setup=='object'
			? stylePkg.setup
			: {}
  }

  const setupStyle = function (withErrSign) {
		// status list [valid, invalid, setup]
		// should return one status above
		// the caller MUST check styleObj.status result
    console.log('setting up', style)
    const sign = withErrSign ? errorsign : ''
    try {
      styleObj.formatter = require(styleEntry)
      styleObj.status = 'valid'
			// res.end should be called by caller for furthur msg
    } catch (err) {
			// check err type
      if (err.code !== 'MODULE_NOT_FOUND') {
        styleObj.status = 'invalid'
        return res.end(JSON.stringify(err))
      }
			// error is module_not_found, run npm setup
			const command = styleSetup.command || 'npm install'
			const timeout = styleSetup.timeout || 60e3
      console.log('setup', style, 'using', command, 'timeout is', timeout);
			// 1 min install
      res.setTimeout(timeout, timeoutFn)
      styleObj.status = 'setting up'
      styleObj.setupProc = exec(command, {cwd: path.join(__dirname, styleFolder)}, function (err, stdout, stderr) {
				delete styleObj.setupProc
        console.log(err, stdout, stderr)
				// now should can safely require
        styleObj.formatter = require(styleEntry)
				// response timeout already?
        if (res.finished) return console.log('setup finished with request timeout')
        if (err) {
          return res.end(sign + '"'+style+'"' + command + ' error \n' + JSON.stringify(err))
        }
        // res.end(sign + command + ' result:\n[stdout]:\n' + stdout + '\n[stderr]:\n' + stderr)
				res.end(sign + '"'+style+'"' + command + ' setup succeed, the fomatter is ready.' )
      })
    }
  }

  if (req.method == 'POST' && segments[1] == 'format' && styleFolder) {
    if (typeof styleObj.formatter !== 'function') {
			// status unknow, get status using setupStyle
      if (!styleObj.status) setupStyle(true)
			// formatter is not valid, halt
      if (styleObj.status != 'valid') {
        if (!res.finished) res.end(errorsign + '"' + style + '"' + ' is in status: ' + styleObj.status)
        return console.log(style, 'formatter invalid')
      }
			// styleObj.formatter now available
    }
    req.on('data', function (data) {
      bodyString += data
    })

    req.on('end', () => {
      let code = bodyString || ''
      // console.log(bodyString)
      code = new Buffer(code, 'base64').toString()
      styleObj.formatter(code, (err, result) => {
        console.log(err, result)
        if (res.finished) return console.log('timeout, but result is', result)
        if (err) res.end(errorsign + JSON.stringify(err))
        else res.end(result)
      })
    })
  }
  if (req.method == 'GET') {
    console.log('GET', req.url, styleFolder, segments)
    switch (segments[1]) {
    case 'setup':
      if (!styleFolder) return res.end('no style configured')
      setupStyle()
      if (styleObj.status == 'valid') {
        res.end('')
      }
      break
    case 'exit':
      console.log('exit now')
      res.end('')
      Object.keys(styles).forEach(style => {
        const obj = styles[style]
        if (obj && obj.setupProc) {
          obj.setupProc.kill()
        }
      })
      setTimeout(_ => {
        process.exit(0)
      }, 300)
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
