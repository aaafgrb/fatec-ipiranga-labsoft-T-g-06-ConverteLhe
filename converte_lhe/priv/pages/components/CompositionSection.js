import Menu from "./NodeMenu.js"
import Grid from "./Grid.js"
import NodeContainer from "./NodeContainer.js";
import DraggedArrow from "./DraggedArrow.js";
import Connector from "./Connector.js";
import { getInputProcess, getOutputProcess, ProcessData, getComposition, getCompShare} from "../functionList.js"
import DraggedConnector from "./DraggedConnector.js";

export default {
  components: {
    Menu,
    Grid,
    NodeContainer,
    DraggedArrow,
    DraggedConnector,
    Connector
  },
  props: {
    startCols: { 
      type: Object, 
      required:true 
    },
    startSpecialRows: { 
      type: Object, 
      required:true 
    },
  },
  inject: ['currentArrowData', 'currentConnectorData', 'newIdentifier'],
  expose: ['updatePositions', 'getComposition', 'getCompShare'],
  data: () => ({
    cols: [],
    specialRows: {},
    processRows: new Map,
    nullProcessRows: []
  }),
  methods:{
    fixCols(){
      const isColEmpty = (cid) => (this.cols[cid].rows.length == 1 && this.cols[cid].rows[0].processData == null) || 
                                  this.cols[cid].rows.length <= 0;
      const isRowEmpty = (cid, rid) => this.cols[cid].rows[rid].processData == null;
      //cols
      for(let i = 0; i < this.cols.length; i++){
        if(!isColEmpty(i)){
          //left
          if(i - 1 < 0 || !isColEmpty(i - 1)){ this.cols.splice(i++, 0, {rows: [{processData: null}]}); }
          let rempty = false;
          //rows
          for(let ii = 0; ii < this.cols[i].rows.length; ii++){
            if(!isRowEmpty(i, ii)){
              //up
              if(ii - 1 < 0 || !isRowEmpty(i, ii - 1)){ this.cols[i].rows.splice(ii++, 0 , {processData: null}); }
              //down
              if(ii + 1 >= this.cols[i].rows.length || !isRowEmpty(i, ii + 1)){ this.cols[i].rows.splice(++ii, 0 , {processData: null}); }
              rempty = false;
            }else{
              //check for repeated empty
              if(rempty){
                this.cols[i].rows.splice(--ii, 1)
              }
              rempty = true;
            }
          }
          //right
          if(i + 1 >= this.cols.length || !isColEmpty(i + 1)){ this.cols.splice(++i, 0, {rows: [{processData: null}]}); }
        }
      }
      let cempty = false;
      for(let i = 0; i < this.cols.length; i++){
        if(isColEmpty(i)){
          if(cempty){ this.cols.splice(--i, 1) }
          cempty = true;
        }else{ cempty = false; }
      }
    },
    onSnap(index){
      //adds dragged process to the processRows
      this.nullProcessRows[index].processData = 
        new ProcessData(this.currentArrowData.currentArrow, this.newIdentifier(), [], null, 
                        [ ...this.currentArrowData.currentArrow.data ])
      this.processRows.set(this.nullProcessRows[index].processData.identifier, this.nullProcessRows[index]);
      this.fixCols();
      Vue.nextTick(this.updatePositions)
    },
    onConnect(inIdentifier, inPortIndex, outIdentifier, outPortIndex){
      this.processRows.get(inIdentifier).processData.connectToInputPort(inPortIndex, this.processRows.get(outIdentifier).processData, outPortIndex)
    },
    onContextMenu(pdata){
      //remove process from composition
      let id = pdata.processData.identifier;
      //ignore if it is the input or output node
      if(id == this.specialRows.input.processData.identifier || id == this.specialRows.output.processData.identifier) return;
      //remove all input connections
      pdata.processData.inPorts.forEach((x, i) => {
        if(x) pdata.processData.removeFromInputPort(this.processRows.get(x.identifier).processData, i)
      })
      //remove all output connections
      pdata.processData.outPorts.forEach(x => {
        x.forEach(y => { if(y) this.processRows.get(y.identifier).processData.removeFromInputPort(pdata.processData, y.portIndex)})
      })
      //remove from processRows
      this.processRows.get(id).processData = null;
      this.processRows.delete(id);
      this.fixCols();
      Vue.nextTick(this.updatePositions)
    },
    updatePositions(){
      if(this.$refs.container){
        this.$refs.container.forEach(x => x.update())
      }
      if(this.$refs.draggedConnector) {this.$refs.draggedConnector.update()}
      
    },
    startDragging(pdata, port, portIndex){
      //checks if its dragging an input port that already has a connection
      if(port.isInput && pdata.processData.inPorts[portIndex]){
        let o = this.processRows.get(pdata.processData.inPorts[portIndex].identifier)
        let i = pdata.processData.inPorts[portIndex].portIndex;
        pdata.processData.removeFromInputPort(o.processData, portIndex)
        this.startDragging(o, o.nodeContainer.$refs.outPorts[i], i)
      }else{
        this.currentConnectorData.identifier = pdata.processData.identifier;
        this.currentConnectorData.portIndex = portIndex;
        this.currentConnectorData.nodeContainer = pdata.nodeContainer;
        this.currentConnectorData.fixedPort = port
        this.currentConnectorData.isDragging = true;
      }
    },
    getComposition(){
      let c = getComposition(this.specialRows.output.processData.inPorts[0], this.processRows);
      if(c == null) c = [];
      let r = "";
      c.forEach(e => r += `${e}/`);
      return r
    },
    getCompShare(){
      return getCompShare(this.specialRows.output, this.processRows);
    }
  },
  mounted(){
    this.cols = this.startCols
    this.specialRows = this.startSpecialRows
    Vue.nextTick(() => {
      let s = this.$refs.grid.getspecialRowPoints();
      this.specialRows.output.point = s.output
      this.specialRows.input.point = s.input
      this.processRows.set(this.specialRows.input.processData.identifier, this.specialRows.input);
      this.processRows.set(this.specialRows.output.processData.identifier, this.specialRows.output);
      this.startCols.forEach(c => c.rows.forEach(r => {
        if(r.processData) this.processRows.set(r.processData.identifier, r);
      }))
      this.fixCols()
    })
  },
  computed: {
    connectors(){
      let r = Array.from(this.processRows)
      .filter(x => x[1].nodeContainer)
      .flatMap((x) => 
        x[1].processData.inPorts.filter(y => y).map(y => (
          { inIdentifier: x[1].processData.identifier, 
            inPortIndex: x[1].processData.inPorts.indexOf(y),
            outIdentifier: y.identifier,
            outPortIndex: y.portIndex,
          })
        )
      )
      return r
    },
  },
  watch: {
  },
  template: `
  <div class="compose-area" style="width: 100%; height: 100%;">
    <grid ref="grid" :cols="cols" :nullProcessRows="nullProcessRows" :processRows="processRows"></grid>
    <svg id="svg" ref="svg">
      <g id="diagram" data-drag="diagram:diagram" data-drag-type="diagram">
        <g id="node-layer">
          <node-container v-for="[k, p] in processRows" :pdata="p" :identifier="k" ref="container" 
            @oncontextmenu="onContextMenu" @startdragging="startDragging"></node-container>
        </g>
        <g id="connections-layer">
          <connector v-for="c in connectors"
            :inPort="processRows.get(c.inIdentifier).nodeContainer.inPorts[c.inPortIndex]" 
            :outPort="processRows.get(c.outIdentifier).nodeContainer.outPorts[c.outPortIndex]"></connector>
          <dragged-connector v-if="currentConnectorData.isDragging" ref="draggedConnector" :processRows="processRows" @onconnect="onConnect"></dragged-connector>
        </g>
      </g>
    </svg>
    <dragged-arrow :points="nullProcessRows" v-if="currentArrowData.isDraggingArrow" @onsnap="onSnap"></dragged-arrow>
  </div>
  `
}