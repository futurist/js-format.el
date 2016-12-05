// #!/usr/bin/env node

const http = require('http')
const format = require('./formatter')

const errorsign = '#!!#'
const port = 58000
const timeout = 10e3  // 10 sec
const server = http.createServer((req, res) => {
  let bodyString = ''

  // prevent node died to set a timeout
  req.resume()
  res.setTimeout(timeout, function () {
    console.log('server', 'request timeout')
    res.end(errorsign + 'js-format server request timeout, try again?')
  })
  res.writeHead(200, {'Content-Type': 'text/plain'})
  if (req.method == 'POST' && req.url == '/format') {
    req.on('data', function (data) {
			bodyString += data
		})

    req.on('end',  () => {
      let code = bodyString || ''
      // console.log(bodyString)
      code = new Buffer(code, 'base64').toString()
			format(code, (err, result) => {
				if (err) res.end(errorsign + JSON.stringify(err))
				else res.end(result)
			})
    })
  }
  if (req.method == 'GET') {
    console.log('GET', req.url)
    switch (req.url) {
    case '/exit':
      console.log('exit now')
      res.end('')
      process.exit(0)
      break
    case '/getport':
      res.end(port + '')
      break
		default:
			res.end('ok', req.url)
    }
  }
}).listen(port, function () {
  console.log('Listening on port', port)
})
