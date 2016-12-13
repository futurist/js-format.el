// #!/usr/bin/env node

const http = require('http')
const path = require('path')
const url = require('url')
const exec = require('child_process').exec
const styles = require('./styles.json')

const errorsign = '#!!#'
const port = 58000
const timeout = 10e3  // 10 sec

// helper function: type checking
function is (object, type) {
  return {}.toString.call(object) === '[object ' + type + ']'
}

function send (res, ...args) {
  if (res.finished) {
    console.log('[response] already sent, the msg:', args)
  } else {
    console.log('[response] with msg:', args)
    res.end(args.join(''))
  }
}

const server = http.createServer((req, res) => {
  let bodyString = ''
  const timeoutFn = withErrSign => socket => {
    console.log('server request timeout')
    const sign = withErrSign ? errorsign : ''
    let msg = ''
    // if setup in process, kill it!
    if (styleObj && styleObj.setupProc) {
      styleObj.status = 'setting up timeout'
      /* some times kill not work, better to let it exit self */
      // styleObj.setupProc.kill('SIGINT')
      // delete styleObj.setupProc
      msg = ' But setup process still run in background. You can kill it manually.'
    }
    send(res, sign, 'js-format server request timeout.', msg)
  }
  req.resume()
  res.writeHead(200, {'Content-Type': 'text/plain'})
  if (req.url == '/') {
    return send(res, 'js-format server')
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
      return send(res, req.method == 'POST' ? errorsign : '', 'there\'s no style configed: "', style, '"')
    }
    // style base folder
    styleFolder = 'styles/' + (styleObj.folder || style)
    styleEntry = './' + styleFolder + '/' + (styleObj.entry || '')
    stylePkg = require('./' + styleFolder + '/package.json')
    // for setup: first use root package, then use style package
    styleSetup = is(styleObj.setup, 'Object')
      ? styleObj.setup
      : (is(stylePkg.setup, 'Object')
         ? stylePkg.setup
         : {})
  }

  const setupStyle = withErrSign => {
    // status list [valid, invalid, setup]
    // should return one status above
    // the caller MUST check styleObj.status result
    if (/^setting up/.test(styleObj.status)) {
      return send(res, '"', style, '"', ' already in status: ', styleObj.status)
    }
    console.log('setting up', style, styleObj.status)
    const sign = withErrSign ? errorsign : ''
    try {
      styleObj.formatter = require(styleEntry)
      styleObj.status = 'valid'
      if (!withErrSign) send(res, '"', style, '"', ' now is the active formatter.')
      // in POST formatter, res.end cannot be called
      // res.end should be called for formatting code
    } catch (err) {
      // check err type
      if (err.code !== 'MODULE_NOT_FOUND') {
        styleObj.status = 'invalid'
        return send(res, JSON.stringify(err))
      }
      // error is module_not_found, run npm setup
      const command = styleSetup.command || 'npm install'
      // timeout: 5 mins enough?
      const timeout = styleSetup.timeout || 5 * 60e3
      console.log('setup', style, 'using', command, 'timeout is', timeout)
      // 1 min install
      res.setTimeout(timeout, timeoutFn(withErrSign))
      styleObj.status = 'setting up. command: ' + command + ', timeout: ' + timeout
      styleObj.setupProc = exec(command, {cwd: path.join(__dirname, styleFolder)}, function (err, stdout, stderr) {
        delete styleObj.setupProc
        console.log(err, stdout, stderr)
        // now should can safely require
        styleObj.formatter = require(styleEntry)
        styleObj.status = 'valid'
        if (err) {
          return send(res, sign + '"' + style + '"' + command + ' error \n' + JSON.stringify(err))
        }
        // res.end(sign + command + ' result:\n[stdout]:\n' + stdout + '\n[stderr]:\n' + stderr)
        send(res, sign + '"' + style + '"' + command + ' setup succeed, the fomatter is ready.')
      })
    }
  }

  if (req.method == 'POST' && segments[1] == 'format' && style) {
    // prevent node died to set a timeout
    res.setTimeout(timeout, timeoutFn(true))
    if (typeof styleObj.formatter !== 'function') {
      // status unknow, get status using setupStyle
      if (!styleObj.status) setupStyle(true)
      // formatter is not valid, halt
      if (styleObj.status != 'valid') {
        return send(res, errorsign + '"' + style + '"' + ' is in status: ' + styleObj.status)
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
        if (err) send(res, errorsign + JSON.stringify(err))
        else send(res, result)
      })
    })
  }
  if (req.method == 'GET') {
    console.log('GET', req.url, styleFolder, segments)
    switch (segments[1]) {
    case 'setup':
      if (!style) return send(res, 'no style configured')
      setupStyle(false)
      if (styleObj.status == 'valid') {
        send(res, '')
      }
      break
    case 'exit':
      console.log('exit now')
      send(res, '')
      Object.keys(styles).forEach(style => {
        const obj = styles[style]
        if (obj && obj.setupProc) {
          obj.setupProc.kill()
          delete obj.setupProc
        }
      })
      setTimeout(_ => {
        process.exit(0)
      }, 300)
      break
    case 'getport':
      send(res, port + '')
      break
    default:
      send(res, 'undefined action', req.url)
    }
  }
}).listen(port, function () {
  console.log('Listening on port', port)
})
