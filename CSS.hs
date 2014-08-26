{-# LANGUAGE QuasiQuotes #-}

module CSS where

import Text.InterpolatedString.Perl6 (q)

css :: String
css = [q|
  body {
    margin: 0;
    padding: 0;
  }
  h1, h2 {
    text-align: center;
  }
  h2 {
    height: 50px;
    font-size: 50px;
    cursor: pointer;
  }
  input {
    text-align: center;
    width: 300px;
    font-size: 200%;
    display: block;
    margin: 1em auto;
  }
  .bar {
    width: 900px;
    margin: 0 auto;
    padding: 10px 0 0 10px;
  }
  .bar span {
    display: inline-block;
    margin: 0 10px 10px 0;
    padding: 0px;
    width: 98px;
    height: 98px;
    border: 1px solid black;
    cursor: pointer;
  }
  .bar span.on {
    background: #FA0;
  }
|]

