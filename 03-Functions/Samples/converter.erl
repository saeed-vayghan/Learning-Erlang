-module(converter).
-export([convert/2]).


convert({fahrenheit, Temp}, celsius) ->
  {celsius, 5 * (Temp - 32) / 9};

convert({celsius, Temp}, fahrenheit) ->
  {farenheit, 32 + Temp * 9 / 5};

convert({reaumur, Temp}, celsius) ->
  {celsius, 10 * Temp / 8};

convert({celsius, Temp}, reaumur) ->
  {reaumur, 8 * Temp / 10};

convert({X, _}, Y) ->
  {cannot, convert, from, X, to, Y}.







% > temp:convert({fahrenheit, 98.6}, celsius).
% {celsius,37.0000}

% > temp:convert({reaumur, 80}, celsius).
% {celsius,100.000}

% > temp:convert({reaumur, 80}, fahrenheit).
% {cannot,convert,reaumur,to,fahrenheit}