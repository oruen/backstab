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

# entrypoint
backstab.start = ->
  backstab.currentMap = null
  backstab.director = new lime.Director(document.body, 1024, 768)
  scene = new lime.Scene()

  #scene.setRenderer(lime.Renderer.CANVAS);
  backstab.director.makeMobileWebAppCapable()

  # set current scene active
  backstab.director.replaceScene scene
  backstab.wsHandler = new backstab.WsHandler(Userinfo.token)
  new backstab.EventHandler(backstab.wsHandler, scene)
  backstab.wsHandler.connect()

backstab.send = (msg) ->
  @wsHandler.send msg

backstab.hideBattleScene = ->
  @director.domElement.style["display"] = "none"

backstab.showBattleScene = ->
  @director.domElement.style["display"] = "block"

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
  @hideBattleScene()
  width = 960
  height = 500
  nodes = planetSystems.map((d) ->
    radius: d.planets.length * 2 + 10
    userId: d.userId
    color: "##{d.color}"
  )
  color = d3.scale.category10()
  force = d3.layout.force().gravity(0.02).charge(-100).nodes(nodes).size([width, height])
  force.start()
  svg = d3.select("body").append("svg").attr("width", width).attr("height", height)
  svg.selectAll("circle").data(nodes).enter().append("svg:circle").attr("r", (d) ->
    d.radius
  ).style("fill", (d, i) ->
    color i % 3
  ).style("stroke", (d, i) ->
    (if d.userId is Userinfo.token then "#000" else color(i % 3))
  ).on("click", ->
    console.log "clicked", arguments_
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

