goog.provide("backstab.PlanetSystem")

backstab.PlanetSystem = (params) ->
  @id = params[1].value
  @userId = params[2].value
  @planets = params[3][0][1]
  @routes = params[3][1][1]
  @
