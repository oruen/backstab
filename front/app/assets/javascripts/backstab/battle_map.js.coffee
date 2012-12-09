goog.provide('backstab.BattleMap')

goog.require('backstab.Planet')
goog.require('backstab.Route')
goog.require('backstab.Army')
goog.require('springy')

class backstab.BattleMap
  constructor: (planetSystem, scene) ->
    @scene = scene
    @planets = []
    @routes = []
    @createPlanets planetSystem
    @createRoutes planetSystem
    @positionPlanets @planets, @routes

  render: (scene) ->
    if @_positioningPlanets
      setTimeout goog.bind(@render, this, scene), 50
    else
      @_render scene

  _render: (scene) ->
    scene = @scene
    @routes.forEach (obj) ->
      scene.appendChild obj.draw()
    @planets.forEach goog.bind((obj) ->
      scene.appendChild obj.draw()
    , this)
    @planets.filter((e) ->
      !!e.userId
    ).forEach goog.bind((obj) ->
      scene.appendChild obj.drawBase()
    , this)

  createPlanets: (planetSystem) ->
    planetSystem.planets.forEach goog.bind((params) ->
      console.log "planet", params
      planet = new backstab.Planet(params)
      @planets.push planet
    , this)

  createRoutes: (planetSystem) ->
    from = undefined
    to = undefined
    planetSystem.routes.forEach goog.bind((params) ->
      console.log "route", params
      from = @planets.filter((p) ->
        p.id is params[1].value
      )[0]
      to = @planets.filter((p) ->
        p.id is params[2].value
      )[0]
      route = new backstab.Route(from, to)
      @routes.push route
    , this)

  positionPlanets: (planets, routes) ->
    graph = new Graph()
    planetNodes = []
    sourceNode = undefined
    destNode = undefined
    planets.forEach (planet) ->
      planetNodes.push graph.newNode(planet)

    routes.forEach (route) ->
      sourceNode = planetNodes.filter((e) ->
        e.data.id is route.source.id
      )[0]
      destNode = planetNodes.filter((e) ->
        e.data.id is route.dest.id
      )[0]
      graph.newEdge sourceNode, destNode
    layout = new Layout.ForceDirected(graph, 400.0, 400.0, 0.5)
    currentBB = layout.getBoundingBox()
    self = this
    @_positioningPlanets = true
    layout.start `undefined`, ->
      layout.eachNode (node, point) ->
        size = currentBB.topright.subtract(currentBB.bottomleft)
        sx = point.p.subtract(currentBB.bottomleft).divide(size.x).x * self.scene.domElement.clientWidth
        sy = point.p.subtract(currentBB.bottomleft).divide(size.y).y * self.scene.domElement.clientHeight
        node.data.x = sx
        node.data.y = sy
      self._positioningPlanets = false
      console.log "done"

  planetById: (id) ->
    @planets.filter((e) ->
      e.id is id
    )[0]
