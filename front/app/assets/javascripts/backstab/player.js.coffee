goog.provide("backstab.Player")

class backstab.Player
  constructor: (record) ->
    params = JSON.parse record.value
    @id = params.email
    @color = params.color
    @name = params.name
