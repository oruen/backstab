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

backstab.requestFight = (params) ->
  planetSystemId = params["planetsystemid"]
  @send Bert.tuple(Bert.atom("global"), Bert.atom("fight"), Bert.binary(planetSystemId))
  $(".js-planetinfo").modal('hide')

backstab.requestDefence = (params) ->
  battleid = params["battleid"]
  console.log params, battleid
  @send Bert.tuple(Bert.atom("global"), Bert.atom("defend"), Bert.binary(battleid))
  $(".js-defendalert").alert("close")

backstab.send = (msg) ->
  @wsHandler.send msg

backstab.renderGlobalMap = (planetSystems) ->
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
  width = 960
  height = 500
  nodes = planetSystems.map((d) ->
    radius: d.planets.length * 2 + 10
    planetsCount: d.planets.length
    userId: d.userId
    color: "##{d.userId && backstab.players[d.userId].color || "EEE"}"
    id: d.id
  )
  force = d3.layout.force().gravity(0.02).charge(-100).nodes(nodes).size([width, height])
  force.start()
  svg = d3.select("body").append("svg").attr("width", width).attr("height", height).classed("global-map", true)
  svg.selectAll("circle").data(nodes).enter().append("svg:circle").attr("r", (d) ->
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
    q = d3.geom.quadtree(nodes)
    i = 0
    n = nodes.length
    q.visit collide(nodes[i])  while ++i < n
    svg.selectAll("circle").attr("cx", (d) ->
      d.x
    ).attr "cy", (d) ->
      d.y

#this is required for outside access after code is compiled in ADVANCED_COMPILATIONS mode
goog.exportSymbol "backstab.start", backstab.start
window.onload = backstab.start

