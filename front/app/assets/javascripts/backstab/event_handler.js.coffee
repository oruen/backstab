goog.provide("backstab.EventHandler")

backstab.EventHandler = (wsHandler) ->
  wsHandler.addEventListener backstab.WsHandler.EventType.MAP, (event) ->
    console.log "Time to draw"
    goog.style.showElement goog.dom.getElementByClass("global-map"), false
    backstab.initDirector()
    backstab.scene = new lime.Scene()
    backstab.scene.setRenderer(lime.Renderer.CANVAS)
    backstab.director.replaceScene backstab.scene
    planetSystem = new backstab.PlanetSystem(event.target)
    backstab.currentMap = new backstab.BattleMap(planetSystem, backstab.scene)
    backstab.currentMap.render()

  wsHandler.addEventListener backstab.WsHandler.EventType.VICTORY, (event) ->
    console.log "Victory", event.target.value
    if Userinfo.id == event.target.value
      $(".js-victory").modal()
    else
      $(".js-defeat").modal()
    goog.style.showElement goog.dom.getElementByClass("lime-director"), false
    goog.style.showElement goog.dom.getElementByClass("global-map"), true

  wsHandler.addEventListener backstab.WsHandler.EventType.GLOBAL_MAP, (event) ->
    console.log "Global map"
    backstab.planetSystems = []
    backstab.players = {}
    event.target[0].forEach (e) ->
      player = new backstab.Player(e)
      backstab.players[player.id] = player
    event.target[1].forEach (e) ->
      backstab.planetSystems.push new backstab.PlanetSystem(e)

    backstab.renderGlobalMap backstab.planetSystems

  wsHandler.addEventListener backstab.WsHandler.EventType.GOTO, (event) ->
    console.log "Goto", event.target
    from = backstab.currentMap.planetById(event.target[0].value)
    to = backstab.currentMap.planetById(event.target[1].value)
    if from.userId == Userinfo.id
      color = [121, 142, 224]
    else
      color = [201, 32, 32]
    army = new lime.Circle().setPosition(from.node.getPosition()).setSize(80, 80).setFill(color[0], color[1], color[2])
    backstab.scene.appendChild army
    anim = new lime.animation.Sequence(new lime.animation.MoveTo(to.node.getPosition()), new lime.animation.FadeTo(0).setDuration(.2))
    goog.events.listen anim, "stop", goog.bind(->
      backstab.scene.removeChild army
    , backstab)
    army.runAction anim

  wsHandler.addEventListener backstab.WsHandler.EventType.POPULATION, (event) ->
    console.log "Populatoin", event.target
    event.target.forEach (e) ->
      planet = backstab.currentMap.planetById(e[0].value)
      population = e[1]
      planet.setQuantity(population)

  wsHandler.addEventListener backstab.WsHandler.EventType.PLANET, (event) ->
    planet = new backstab.Planet(event.target)
    console.log "Planet", planet
    existingPlanet = backstab.currentMap.planetById(planet.id)
    existingPlanet.setQuantity(planet.quantity)
    existingPlanet.setUser(planet.userId)

  wsHandler.addEventListener backstab.WsHandler.EventType.ASSAULT, (event) ->
    button = goog.dom.getElementByClass("js-defend")
    goog.dom.dataset.set button, "battleid", event.target.value
    $(".js-defendalert").removeClass("hide")

  wsHandler.addEventListener backstab.WsHandler.EventType.PLANET_SYSTEM, (event) ->
    planetSystem = new backstab.PlanetSystem(event.target)
    backstab.updatePlanetSystem(planetSystem)


