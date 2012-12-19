goog.provide('backstab')

goog.require('lime.Director')
goog.require('lime.Scene')
goog.require('lime.Layer')
goog.require('lime.Circle')
goog.require('lime.Label')
goog.require('lime.animation.Spawn')
goog.require('lime.animation.FadeTo')
goog.require('lime.animation.ScaleTo')
goog.require('lime.animation.MoveTo')
goog.require('backstab.WsHandler')
goog.require('backstab.Bert')
goog.require('backstab.Planet')
goog.require('backstab.BattleMap')
goog.require('backstab.PlanetSystem')
goog.require('backstab.Player')
goog.require('backstab.EventHandler')
goog.require('d3')
goog.require('goog.dom')
goog.require('goog.dom.dataset')

# entrypoint
backstab.start = ->
  backstab.Planet.loadGraphics()
  attackButton = goog.dom.getElementByClass('js-attack')
  defendButton = goog.dom.getElementByClass('js-defend')
  goog.events.listen attackButton, goog.events.EventType.CLICK, ->
    backstab.requestFight(goog.dom.dataset.getAll(attackButton))
  goog.events.listen defendButton, goog.events.EventType.CLICK, ->
    backstab.requestDefence(goog.dom.dataset.getAll(defendButton))
  backstab.currentMap = null
  backstab.wsHandler = new backstab.WsHandler(Userinfo.token)
  new backstab.EventHandler(backstab.wsHandler)
  backstab.wsHandler.connect()

backstab.initDirector = ->
  if @director
    goog.style.showElement goog.dom.getElementByClass("lime-director"), true
  else
    @director = new lime.Director(document.body, 1024, 768)
    @director.makeMobileWebAppCapable()

backstab.requestFight = (params) ->
  planetSystemId = params["planetsystemid"]
  @send Bert.tuple(Bert.atom("global"), Bert.atom("fight"), Bert.binary(planetSystemId))
  $(".js-planetinfo").modal('hide')

backstab.requestDefence = (params) ->
  battleid = params["battleid"]
  console.log params, battleid
  @send Bert.tuple(Bert.atom("global"), Bert.atom("defend"), Bert.binary(battleid))
  $(".js-defendalert").alert("close")

backstab.updatePlanetSystem = (planetSystem) ->
  existingPlanetSystem = @planetSystems.filter((e) -> e.id == planetSystem.id)[0]
  if existingPlanetSystem
    existingPlanetSystem.updateUserId(planetSystem.userId)
  else
    @planetSystems.push planetSystem
    @nodes.push planetSystem.visData()
    @doRenderGlobalMap()

backstab.updatePlayer = (player) ->
  backstab.players[player.id] = player

backstab.send = (msg) ->
  @wsHandler.send msg

backstab.renderGlobalMap = (planetSystems) ->
  backstab.nodes = planetSystems.map((d) -> d.visData())
  svg = d3.select("body").append("svg").attr("width", 960).attr("height", 500).classed("global-map", true)
  backstab.doRenderGlobalMap()

backstab.doRenderGlobalMap = () ->
  collide = (node) ->
    r = node.radius + 40
    nx1 = node.x - r
    nx2 = node.x + r
    ny1 = node.y - r
    ny2 = node.y + r
    (quad, x1, y1, x2, y2) ->
      if quad.point and (quad.point isnt node)
        x = node.x - quad.point.x
        y = node.y - quad.point.y
        l = Math.sqrt(x * x + y * y)
        r = node.radius + quad.point.radius
        if l < r
          l = (l - r) / l * .5
          node.x -= x *= l
          node.y -= y *= l
          quad.point.x += x
          quad.point.y += y
      x1 > nx2 or x2 < nx1 or y1 > ny2 or y2 < ny1
  svg = d3.select("svg")
  width = parseInt svg.attr("width"), 10
  height = parseInt svg.attr("height"), 10
  force = d3.layout.force().gravity(0.02).charge(-100).nodes(backstab.nodes).size([width, height])
  force.start()
  svg.selectAll("circle").data(backstab.nodes).enter().append("svg:circle").attr("r", (d) ->
    d.radius
  ).style("fill", (d, i) ->
    d.color
  ).style("stroke", (d, i) ->
    (if d.userId is Userinfo.id then "#000" else d.color)
  ).on("click", (d) ->
    attackButton = goog.dom.getElementByClass("js-attack")
    goog.dom.dataset.set attackButton, "planetsystemid", d.id
    if d.userId
      if d.userId == Userinfo.id
        goog.dom.getElementByClass("js-header").innerText = "This planet system belongs to you"
        goog.style.showElement attackButton, false
      else
        goog.dom.getElementByClass("js-header").innerText = "This planet system belongs to #{backstab.players[d.userId].name}"
        goog.style.showElement attackButton, true
    else
      goog.style.showElement attackButton, true
      goog.dom.getElementByClass("js-header").innerText = "This planet system is abandoned"
    goog.dom.getElementByClass("js-body").innerText = "It has #{d.planetsCount} planets. Some outlaws may hide at some of them."
    $(".js-planetinfo").modal({keyboard: true})
  ).call force.drag
  force.on "tick", (e) ->
    q = d3.geom.quadtree(backstab.nodes)
    i = 0
    n = backstab.nodes.length
    q.visit collide(backstab.nodes[i])  while ++i < n
    svg.selectAll("circle").attr("cx", (d) ->
      d.x
    ).attr "cy", (d) ->
      d.y

#this is required for outside access after code is compiled in ADVANCED_COMPILATIONS mode
goog.exportSymbol "backstab.start", backstab.start
window.onload = backstab.start

