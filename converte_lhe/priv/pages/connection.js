import ArrowSection from './components/ArrowSection.js'
import CompositionSection from './components/CompositionSection.js'
import DataSection from './components/DataSection.js'
import NodeMenu from './components/NodeMenu.js'
import { throttle } from './Util.js'
import { getArrowList, ProcessData, getOutputProcess, getInputProcess } from "./functionList.js"

Vue.createApp({
  components: {
    ArrowSection,
    DataSection,
    CompositionSection,
    NodeMenu
  },
  data: () => ({
    resizeObserver: null,
    mouseSvgPos: {x: 0, y: 0},
    mousePagePos: {x: 0, y: 0},
    currentArrowData: { currentArrow: null, isDraggingArrow: false, callback: null},
    currentConnectorData: { isDragging: false, nodeContainer: null, fixedPort: null, callback: null },
    currentMenuData: { isOpen: false, processData: null},
    identifier: 2, //id 0 = output | id 1 = input
    ticking: false,
    cols:
    [ { rows: 
        [ { processData: null
          }
        ]
      }
    ],
    specialRows: {output: { processData: new ProcessData(getOutputProcess(), 0, [], null, []) },
                  input: { processData: new ProcessData(getInputProcess(), 1, [], null, [])} }
  }),
  provide(){
    return {
      mouseSvgPos: this.mouseSvgPos,
      mousePagePos: this.mousePagePos,
      currentArrowData: this.currentArrowData,
      currentConnectorData: this.currentConnectorData,
      currentMenuData: this.currentMenuData,
      newIdentifier: this.newIdentifier,
      getComposition: this.getComposition,
      getCompShare: this.getCompShare
    }
  },
  mounted(){
    const queryString = window.location.search;
    const urlParams = new URLSearchParams(queryString);
    const comp = urlParams.get('comp')
    this.setComp(comp)

    window.addEventListener('mousemove', this.updateMousePosition);
    window.addEventListener('mouseup', this.onMouseUp);
    this.resizeObserver = new ResizeObserver(throttle(this.onCompResized, 200));
    this.resizeObserver.observe(this.$refs.compArea)
  },
  beforeDestroy() {
    this.resizeObserver.disconnect();
    window.removeEventListener('mouseup', this.onMouseUp);
    window.removeEventListener('mousemove', this.updateMousePosition);
  },
  methods: {
    newIdentifier(){
      return this.identifier++
    },
    updateMousePosition(event){
      if (!this.ticking) {
        window.requestAnimationFrame(() => {
          if(this.$refs.compArea){
            this.mouseSvgPos.x = event.clientX + window.scrollX + this.$refs.compArea.scrollLeft;
            this.mouseSvgPos.y = event.clientY + window.scrollY + this.$refs.compArea.scrollTop;
          }
          this.mousePagePos.x = event.clientX + window.scrollX;
          this.mousePagePos.y = event.clientY + window.scrollY;
  
          this.ticking = false;
        });
        this.ticking = true;
      }
    },
    onMouseUp(event){
      if(this.currentArrowData.isDraggingArrow){
        if(this.currentArrowData.callback) this.currentArrowData.callback(event);
        this.currentArrowData.isDraggingArrow = false;
      }
      if(this.currentConnectorData.isDragging){
        if(this.currentConnectorData.callback) this.currentConnectorData.callback(event);
        this.currentConnectorData.isDragging = false;
      }
    },
    onStartDragArrow(arrow){
      this.currentArrowData.currentArrow = arrow;
      this.currentArrowData.isDraggingArrow = true;
    },
    onCompResized(mutation){
      this.$refs.comp.updatePositions();
    },
    getComposition(){
      let r = this.$refs.comp.getComposition()
      console.log(`generated composition: ${r}`)
      return r
    },
    getCompShare(){
      return this.$refs.comp.getCompShare()
    },
    setComp(comp){
      if(!comp){ return }
      let keys = ['nodeId', 'processId', 'processParams', 'connections']
      let sections = comp.split(/(?<!\\)\|/)
      let objects = sections.map(x => 
        x.split(/(?<!\\)\//).reduce((obj, v, id) => {
          obj[keys[id]] = v; return obj
        }, {})
      )
      objects.map(x => {
        x.nodeId = parseInt(x.nodeId);
        x.processId = parseInt(x.processId);
        x.processParams = x.processParams == '' ? [] : x.processParams.split(/(?<!\\),/).map(y => y.replace(/\\([,\\\/|:])/g, '$1'));
        
        x.connections = x.connections == '' ? [] : x.connections.split(/(?<!\\),/).map(xx => xx.split(/(?<!\\):/))
          .map(xx => ({identifier: parseInt(xx[0]), portIndex: parseInt(xx[1])}));
      })
      let out = objects.filter(x => x.processId == 0)[0]
      let oldProcesses = new Map(objects.map(x => [x.nodeId, x]))
      let processList = getArrowList();

      let dict = new Map() //relation of old node identifiers and new node identifiers
      let newProcesses = new Map() //created processess list

      //create processess
      const createNodesLoop = (conn) => {
        let oNode = oldProcesses.get(conn.identifier)

        //ignore if it is the input node
        if(oNode.processId == 1){
          return 1;
        }

        let rId = this.newIdentifier()
        dict.set(oNode.nodeId, rId);

        //create node
        let r = new ProcessData(processList.get(oNode.processId), rId, [], null, oNode.processParams)

        //create connections
        oNode.connections.forEach((x, i) => {  
          if(!Number.isInteger(x.identifier)) return
          //checks if the node was already created
          let id = dict.has(x.identifier) ? dict.get(x.identifier) : createNodesLoop(x);
          let op = id == 1 ? this.specialRows.input : newProcesses.get(id)

          r.connectToInputPort(i, op.processData, x.portIndex)
        })
        newProcesses.set(rId, {processData: r})
        return rId;
      }
      if(out.connections.length <= 0){ return; } 
      createNodesLoop(out.connections[0])
      this.specialRows.output.processData.connectToInputPort(0, newProcesses.get(dict.get(out.connections[0].identifier)).processData, out.connections[0].portIndex)

      //dict of the current col position of each node
      //  positions are relative to the end of the array
      let posDict = new Map();
      //organize processess on cols and rows
      const addToPos = (pos, val) => {
        let len = this.cols.length
        if(len - 1 - pos <= 0){ this.cols.splice(len - 1 - pos, 0, {rows: [val]})
        }else{                  this.cols[len - 2 - pos].rows.push(val)
        }
        posDict.set(val.processData.identifier, {c: pos, r: this.cols[len - 1 - pos].rows.length - 1})
      }
      const organizeNodesLoop = (colPos, conn) => {
        if(conn.identifier == 1){ return }
        let oNode = newProcesses.get(conn.identifier)
        if(posDict.has(oNode.processData.identifier)){
          let {c, r} = posDict.get(conn.identifier)
          //if the process that the connections comes from already exists and is ahead (towards the end) of this node
          //  delete it and put it behind this node
          if(c < colPos){ 
            this.cols[this.cols.length - 2 - c].rows.splice(r, 1)
            addToPos(colPos, oNode)
          }
        }else{
          addToPos(colPos, oNode)
        }
        oNode.processData.inPorts.forEach(x => organizeNodesLoop(colPos + 1, x))
      }
      organizeNodesLoop(0, this.specialRows.output.processData.inPorts[0])
    }
  },
  template: `
  <div class="row" id="comp-area" style="resize: vertical; overflow: auto; height: 50vh; justify-content: center;" ref="compArea">
    <div class="gx-0" style="width: 100%;">
      <composition-section ref="comp" :startCols="cols" :startSpecialRows="specialRows"></composition-section>
    </div>
  </div>
  <div class="row" style="resize: vertical; overflow: auto; height: 50vh;" >
    <div class="col-md text-bg-secondary">
      data
      <data-section></data-section>
    </div>
    <div class="col-md text-bg-success">
      arrows
      <arrow-section @onstartdrag="onStartDragArrow"></arrow-section>
    </div>
  </div>
  <node-menu v-if="currentMenuData.isOpen"></node-menu>
  `
}).mount('#app')


