//adapted from
//https://gsap.com/community/forums/topic/17157-drawing-drag-and-drop-connectors-between-svg-elements/?do=findComment&comment=76819

SVGElement.prototype.getTransformToElement = 
  SVGElement.prototype.getTransformToElement || 
  function (toElement) {
    return toElement.getScreenCTM().inverse().multiply(this.getScreenCTM());
  };

//
// CONNECTOR
// ===========================================================================
class Connector {

  constructor() {

    this.id = `connector_${++nextUid}`;
    this.dragType = "connector";
    this.isSelected = false;
    this.element = connectorElement.cloneNode(true);
    this.path = this.element.querySelector(".connector-path");
    this.pathOutline = this.element.querySelector(".connector-path-outline");
    this.inputHandle = this.element.querySelector(".input-handle");
    this.outputHandle = this.element.querySelector(".output-handle");
  }

  init(port) {
    connectorLayer.appendChild(this.element);
    this.setStaticPort(port);

  }

  setStaticPort(port, nonStaticPort = null){
    this.isInput = port.isInput;

    if (port.isInput) {
      this.inputPort = port;
      this.dragElement = this.outputHandle;
      this.staticElement = this.inputHandle;
    } else {
      this.outputPort = port;
      this.dragElement = this.inputHandle;
      this.staticElement = this.outputHandle;
    }

    this.staticPort = port;
    this.dragElement.setAttribute("data-drag", `${this.id}:connector`);
    this.staticElement.setAttribute("data-drag", `${port.id}:port`);

    if(nonStaticPort == null){
      TweenLite.set([this.inputHandle, this.outputHandle], {
        x: port.global.x,
        y: port.global.y
      });
    }else{
      TweenLite.set([this.outputHandle], 
        nonStaticPort.isInput 
        ? { x: port.global.x, y: port.global.y} 
        : { x: nonStaticPort.global.x, y: nonStaticPort.global.y}
      );
      TweenLite.set([this.inputHandle], 
        nonStaticPort.isInput 
        ? { x: nonStaticPort.global.x, y: nonStaticPort.global.y} 
        : { x: port.global.x, y: port.global.y}
      );
    }

    
  }

  updatePath() {

    const x1 = this.inputHandle._gsTransform.x;
    const y1 = this.inputHandle._gsTransform.y;

    const x4 = this.outputHandle._gsTransform.x;
    const y4 = this.outputHandle._gsTransform.y;

    const dx = Math.abs(x1 - x4) * bezierWeight;

    const p1x = x1;
    const p1y = y1;

    const p2x = x1 - dx;
    const p2y = y1;

    const p4x = x4;
    const p4y = y4;

    const p3x = x4 + dx;
    const p3y = y4;

    const data = `M${p1x} ${p1y} C ${p2x} ${p2y} ${p3x} ${p3y} ${p4x} ${p4y}`;

    this.path.setAttribute("d", data);
    this.pathOutline.setAttribute("d", data);
  }

  updateHandle(port) {

    if (port === this.inputPort) {

      TweenLite.set(this.inputHandle, {
        x: port.global.x,
        y: port.global.y
      });


    } else if (port === this.outputPort) {

      TweenLite.set(this.outputHandle, {
        x: port.global.x,
        y: port.global.y
      });

    }

    this.updatePath();
  }

  placeHandle() {
    const skipShape = this.staticPort.parentNode.element;

    let hitPort;

    for (let shape of shapes) {

      if (shape.element === skipShape) {
        continue;
      }

      if (Draggable.hitTest(this.dragElement, shape.element)) {

        const ports = this.isInput ? shape.outputPorts : shape.inputPorts;

        for (let port of ports) {

          if (Draggable.hitTest(this.dragElement, port.portElement)) {
            hitPort = port;
            break;
          }
        }

        if (hitPort) {
          break;
        }
      }
    }

    if (hitPort) {

      
      if (this.isInput) {
        this.outputPort = hitPort;
      } else {
        this.inputPort = hitPort;
      }

      this.dragElement.setAttribute("data-drag", `${hitPort.id}:port`);

      hitPort.addConnector(this);
      this.updateHandle(hitPort);

      this.inputPort.parentNode.onConnect(this);

    } else {
      this.remove();
    }
  }


  remove() {

    if (this.inputPort) {
      this.inputPort.removeConnector(this);
    }

    if (this.outputPort) {
      this.outputPort.removeConnector(this);
    }

    this.isSelected = false;

    this.path.removeAttribute("d");
    this.pathOutline.removeAttribute("d");
    this.dragElement.removeAttribute("data-drag");
    this.staticElement.removeAttribute("data-drag");

    this.staticPort = null;
    this.inputPort = null;
    this.outputPort = null;
    this.dragElement = null;
    this.staticElement = null;

    connectorLayer.removeChild(this.element);
    connectorPool.push(this);
  }

  onDrag() {
    this.updatePath();
  }

  onDragEnd() {
    this.placeHandle();
  }
}


//
// NODE PORT
// =========================================================================== 
class NodePort {

  constructor(parentNode, isInput, i, label, subtitle, y) {

    this.id = `port_${++nextUid}`;
    this.dragType = "port";
    
    this.parentNode = parentNode;
    this.isInput = isInput;
    this.i = i;
    this.label = label;
    this.subtitle = subtitle;

    this.element = isInput ? inputFieldElement.cloneNode(true) : outputFieldElement.cloneNode(true);

    this.element.setAttribute("transform", `translate(0, ${y})`);
    this.element.querySelector(".port-label").innerHTML = label;
    this.element.querySelector(".port-subtitle").innerHTML = subtitle;

    this.parentNode.element.querySelector(isInput ? ".inputs" : ".outputs").appendChild(this.element);

    this.portElement = this.element.querySelector(".port");
    this.portScrim = this.element.querySelector(".port-scrim");

    this.portScrim.setAttribute("data-drag", `${this.id}:port`);

    this.connectors = [];
    this.lastConnector;

    const bbox = this.portElement.getBBox();

    this.global = svg.createSVGPoint();
    this.center = svg.createSVGPoint();
    this.center.x = bbox.x + bbox.width / 2;
    this.center.y = bbox.y + bbox.height / 2;

    this.update();
  }

  getConnector() {

    let connector;

    if(this.isInput && this.connectors.length != 0){
      connector = this.connectors[0];
      connector.inputPort.removeConnector(connector);
      connector.setStaticPort(connector.outputPort, connector.inputPort);
    }else{

      if (connectorPool.length) {
        connector = connectorPool.pop();
        connectorLookup[connector.id] = connector;
      } else {
        connector = new Connector();
      }
      connector.init(this);
      this.connectors.push(connector);
    }

    this.lastConnector = connector;
  }

  removeConnector(connection) {

    const index = this.connectors.indexOf(connection);

    if (index > -1) {
      this.connectors.splice(index, 1);
    }
  }

  addConnector(connection) {
    if(this.isInput){
      this.connectors.forEach(c => c.remove());
    }
    this.connectors.push(connection);
  }

  update() {

    const transform = this.portElement.getTransformToElement(diagramElement);
    this.global = this.center.matrixTransform(transform);

    for (let connector of this.connectors) {
      connector.updateHandle(this);
    }
  }
}


//
// NODE SHAPE
// =========================================================================== 
class NodeShape {

  constructor(posElement, template) {
    this.posElement = posElement;
    this.template = template;
    this.menuData = structuredClone(template.menu);

    this.id = `shape_${++nextUid}`;
    this.dragType = "shape";

    this.inputPorts = [];
    this.outputPorts = [];

    this.element = nodeContainerElement.cloneNode(true);
    this.element.querySelector(".node-header").style.fill = template.color;
    nodeLayer.appendChild(this.element);

    this.element.setAttribute("data-drag", `${this.id}:shape`);

    let offset = 50;
    let height = 50;
  
    this.element.querySelector(".header-title").innerHTML = template.label;
  
    let inPortCount = template.inPorts.length;
    let inGap = inPortCount <= 1 ? 0 : height / (inPortCount - 1);
    for(let i = 0; i < inPortCount; i++){
      const port = new NodePort(this, true, i, template.inPorts[i].label, template.inPorts[i].subtitle, offset + i * inGap);
      portLookup[port.id] = port;
      ports.push(port);
      this.inputPorts.push(port);
    }
    
  
    let outPortCount = template.outPorts.length;
    let outGap = outPortCount <= 1 ? 0 : height / (outPortCount - 1);
    for(let i = 0; i < outPortCount; i++){
      const port = new NodePort(this, false, i, template.outPorts[i].label, template.outPorts[i].subtitle, offset + i * outGap);
      portLookup[port.id] = port;
      ports.push(port);
      this.outputPorts.push(port);
    }

    this.element.addEventListener("contextmenu", (e) => {
      removeCrow(this.posElement);
      this.remove();
      e.preventDefault();
    });

    shapeLookup[this.id] = this;
    shapes.push(this);
  }

  update(){
    let {x, y} = this.posElement.getBoundingClientRect();
    let {width: ewidth, height: eheight} = this.element.getBoundingClientRect();

    let {x: areaX, y: areaY} = composeArea.getBoundingClientRect();

    TweenLite.set(this.element, { 
      x: x - ewidth / 2 - areaX, 
      y: y - eheight / 2 - areaY
    });

    for (let input of this.inputPorts) {
      input.update();
    }

    for (let output of this.outputPorts) {
      output.update();
    }
  }

  remove(){
    this.inputPorts.forEach(function(x) { 
      x.connectors.forEach(c => c.remove())
      portLookup[x.id] = null;
      x.parentNode = null;
      let index = ports.indexOf(x);
      if (index > -1) { 
        ports.splice(index, 1); 
      }
    });

    this.outputPorts.forEach(function(x) { 
      x.connectors.forEach(c => c.remove())
      portLookup[x.id] = null;
      x.parentNode = null;
      let index = ports.indexOf(x);
      if (index > -1) { 
        ports.splice(index, 1); 
      }
    });

    nodeLayer.removeChild(this.element);
    shapeLookup[this.id] = null;
    let i = shapes.indexOf(this);
    if (i > -1) {
      shapes.splice(i, 1); 
    }
  }

  onConnect(connection){
    //console.log(connection.inputPort, connection.outputPort);
  }

  showMenu(){
    if(this.menuData.length == 0) return;
    if(menuHolder == this){
      hideMenu();
      return;
    }
    this.posElement.appendChild(menuPopupElement);
    clearMenu();
    populateMenu(this.menuData);

    menuPopupElement

    menuPopupElement.style.visibility = "visible";
    menuHolder = this;
  }

  onCloseMenu(){
    Array.from(menuPopupElement.children).forEach((f, i) => this.menuData[i].data = f.lastElementChild.value);
  }
}

//
// MENU
//===========================================================================
function hideMenu(){
  menuPopupElement.style.visibility = "hidden";
  menuHolder.onCloseMenu();
  menuHolder = null;
}

function clearMenu(){
  menuPopupElement.innerHTML = '';
}

function populateMenu(menuData){
  menuData.forEach(function(x){
    let g = menuGroupElement.cloneNode(true);
    g.firstElementChild.innerHTML = x.label;
    g.lastElementChild.value = x.data;
    menuPopupElement.appendChild(g);
  })
}

//
// DIAGRAM
// ===========================================================================
class Diagram {

  constructor() {

    this.dragElement = this.element = diagramElement;
    this.target = null;
    this.targetDragType = null;
    this.isDragging = false;

    this.dragTarget = this.dragTarget.bind(this);
    this.prepareTarget = this.prepareTarget.bind(this);
    this.stopDragging = this.stopDragging.bind(this);

    this.draggable = new Draggable(dragProxy, {
      allowContextMenu: true,
      trigger: svg,
      onDrag: this.dragTarget,
      onDragEnd: this.stopDragging,
      onPress: this.prepareTarget
    });

  }

  stopDragging() {
    this.target && this.target.onDragEnd && this.target.onDragEnd();
    this.isDragging = false;
  }

  prepareTarget(event) {

    let element = event.target;
    let drag;

    while (!(drag = element.getAttribute("data-drag")) && element !== svg) {
      element = element.parentNode;
    }

    drag = drag || "diagram:diagram";
    const split = drag.split(":");
    this.dragId = split[0];
    this.dragType = split[1];

    if(this.dragType == "shape"){
      if(menuHolder != null){ menuHolder.onCloseMenu(); }
      shapeLookup[this.dragId].showMenu();
    }else{
      if(menuHolder != null){ hideMenu(); }
    }
  }

  startDrag(){
    switch (this.dragType) {
      case "port":
        const port = portLookup[this.dragId];
        port.getConnector();
        this.target = port.lastConnector;
        this.targetDragType = this.target.dragType;
        break;

      case "connector":
        this.target = connectorLookup[this.dragId];
        break;

      default:
        this.target = null;
        break;
    }
    this.isDragging = true;
  }

  dragTarget() {
    //this is necessary to prevent the creation of the connector on case that the user only clicks but doesnt drag
    if(this.isDragging == false) this.startDrag();

    if(this.target == null) return;

    TweenLite.set(this.target.dragElement, {
      x: `+=${this.draggable.deltaX}`,
      y: `+=${this.draggable.deltaY}`
    });


    this.target.onDrag && this.target.onDrag();
  }
}


//
// API
// ===========================================================================

function createNodeShape(template, posElement) {
  const shape = new NodeShape(posElement, template);
}

function updateConnections(){
  shapes.forEach(s => s.update());
}

function getComposition(){

}

//
// APP
// ===========================================================================
let nextUid = 0;

const bezierWeight = 0.675;

const svg = document.querySelector("#svg");
const diagramElement = document.querySelector("#diagram");

const shapeLookup = {};
const portLookup = {};
const connectorLookup = {};

const ports = [];
const shapes = [];
const connectorPool = [];

const dragProxy = document.querySelector("#drag-proxy");

const frag = document.createDocumentFragment();
frag.appendChild(document.querySelector(".connector"));
frag.appendChild(document.querySelector(".node-container"));
frag.appendChild(document.querySelector(".input-field"));
frag.appendChild(document.querySelector(".output-field"));
frag.appendChild(document.querySelector(".menu-group"));
const connectorElement = frag.querySelector(".connector");
const nodeContainerElement = frag.querySelector(".node-container");
const inputFieldElement = frag.querySelector(".input-field");
const outputFieldElement = frag.querySelector(".output-field");
const menuGroupElement = frag.querySelector(".menu-group");

const composeArea = document.querySelector('.compose-area');
const connectorLayer = document.querySelector("#connections-layer");
const nodeLayer = document.querySelector("#node-layer");

const menuPopupElement = document.querySelector(".menu-container");
let menuHolder = null;

const diagram = new Diagram();

new NodeShape(document.querySelector(".srow-input"), 
  { label: "input"
  , color: "#d63c3c"    
  , menu: []
  , inPorts: []
  , outPorts: [{subtitle: "string", label: "input"}]
  });

new NodeShape(document.querySelector(".srow-output"), 
  { label: "output"
  , color: "#d63c3c"
  , menu: []
  , inPorts: [{subtitle: "value", label: "output"}]
  , outPorts: []
  });
  