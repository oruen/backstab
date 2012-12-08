goog.provide("backstab.Player")

class backstab.Player
  constructor: (params) ->
    @name = params[0][1]
    @id = params[1][1]
    @color = params[2][1]
