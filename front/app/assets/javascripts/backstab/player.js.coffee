goog.provide("backstab.Player")

class backstab.Player
  constructor: (params) ->
    @name = params[0][1].value
    @id = params[1][1].value
    @color = params[2][1].value
