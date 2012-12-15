goog.provide("backstab.PlanetSystem")

class backstab.PlanetSystem
  constructor: (params) ->
    @id = params[1].value
    @userId = params[2].value
    @planets = params[3][0][1]
    @routes = params[3][1][1]

  updateUserId: (val) ->
    @userId = val
    $.extend(backstab.nodes.filter((e) => e.id == @id)[0], @visData())
    d3.select("svg").selectAll("circle").
      data(backstab.nodes).
      style("fill", (d, i) -> d.color).
      style("stroke", (d, i) -> (if d.userId is Userinfo.id then "#000" else d.color))

  visData: ->
    radius: @planets.length * 2 + 10
    planetsCount: @planets.length
    userId: @userId
    color: "##{@userId && backstab.players[@userId].color || "EEE"}"
    id: @id
