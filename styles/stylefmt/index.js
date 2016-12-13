var postcss = require('postcss')
var scss = require('postcss-scss') // when you use scss syntax
var stylefmt = require('stylefmt')

function format (code, cb) {
  postcss([
    stylefmt
  ]).process(code, {
    syntax: scss
  }).then(function (result) {
    cb(null, result.css)
  }).catch(cb)
}

module.exports = format
