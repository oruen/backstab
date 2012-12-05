goog.provide('backstab.BattleMap');

goog.require('backstab.Planet');
goog.require('backstab.Route');
goog.require('backstab.Army');
goog.require('springy');

backstab.BattleMap = function(mapInfo, scene) {
  this.scene = scene;
  this.planets = [];
  this.routes = [];

  this.createPlanets(mapInfo);
  this.createRoutes(mapInfo);
  this.positionPlanets(this.planets, this.routes);
};

backstab.BattleMap.prototype.render = function(scene) {
  if (this._positioningPlanets) {
    setTimeout(goog.bind(this.render, this, scene), 50);
  } else {
    this._render(scene);
  }
};

backstab.BattleMap.prototype._render = function(scene) {
  var scene = this.scene;
  this.routes.forEach(function(obj) {
    scene.appendChild(obj.draw());
  });
  this.planets.forEach(goog.bind(function(obj) {
    scene.appendChild(obj.draw());
  }, this));
  this.planets.filter(function(e) {return !!e.userId;}).forEach(goog.bind(function(obj){
    scene.appendChild(obj.drawBase());
  }, this));
};

backstab.BattleMap.prototype.createPlanets = function(mapInfo) {
  var planetsInfo = mapInfo.filter(function(e) {return e[0].value === "planets"})[0][1].map(function(e) {return e.value;});
  planetsInfo.forEach(goog.bind(function(params) {
    console.log("planet", params);
    var planet = new backstab.Planet(params);
    this.planets.push(planet);
  }, this));
};

backstab.BattleMap.prototype.createRoutes = function(mapInfo) {
  var routesInfo = mapInfo.filter(function(e) {return e[0].value === "routes"})[0][1].map(function(e) {return e.value;});
  var from, to;
  routesInfo.forEach(goog.bind(function(params) {
    console.log("route", params);
    from = this.planets.filter(function(p) {return p.id == params[1].value})[0];
    to = this.planets.filter(function(p) {return p.id == params[2].value})[0];
    var route = new backstab.Route(from, to);
    this.routes.push(route);
  }, this));
};

backstab.BattleMap.prototype.positionPlanets = function(planets, routes) {
  var graph = new Graph(),
      planetNodes = [],
      sourceNode, destNode;

  planets.forEach(function(planet) {
    planetNodes.push(graph.newNode(planet));
  });
  routes.forEach(function(route) {
    sourceNode = planetNodes.filter(function(e){ return e.data.id === route.source.id })[0];
    destNode = planetNodes.filter(function(e){ return e.data.id === route.dest.id })[0];
    graph.newEdge(sourceNode, destNode);
  });
  var layout = new Layout.ForceDirected(graph, 400.0, 400.0, 0.5);
  var currentBB = layout.getBoundingBox();
  var self = this;
  this._positioningPlanets = true;
  layout.start(undefined, function() {
    layout.eachNode(function(node, point) {
      var size = currentBB.topright.subtract(currentBB.bottomleft);
      var sx = point.p.subtract(currentBB.bottomleft).divide(size.x).x * self.scene.domElement.clientWidth;
      var sy = point.p.subtract(currentBB.bottomleft).divide(size.y).y * self.scene.domElement.clientHeight;
      node.data.x = sx;
      node.data.y = sy;
		})
    self._positioningPlanets = false;
    console.log("done");
  });
};

backstab.BattleMap.prototype.planetById = function(id) {
  return this.planets.filter(function(e) { return e.id === id})[0];
};
