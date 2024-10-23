import Menu from "./NodeMenu.js"
import Grid from "./Grid.js"
import NodeContainer from "./NodeContainer.js";
import DraggedArrow from "./DraggedArrow.js";
import Connector from "./Connector.js";
import { getInputProcess, getOutputProcess } from "../functionList.js"
import { getComposition } from '../functionList.js'
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
    startCols: 
    { type: Object
    , default:
      [ { rows: 
          [ { process: null,
              processData: {
                identifier: null,
                inPorts: [],
                outPorts: [],
                data: null
              }
            }
          ]
        }
      ]
    }
  },
  inject: ['currentArrowData', 'currentConnectorData', 'newIdentifier'],
  expose: ['updatePositions', 'getComposition'],
  data: () => ({
    cols: [],
    specialRows: {input: {process: null, processData: {identifier: null, inPorts: [], outPorts: [], data: null}}, 
                  output: {process: null, processData: {identifier: null, inPorts: [], outPorts: [], data: null}}},
    processRows: new Map,
    nullProcessRows: []
  }),
  methods:{
    fixCols(){
      const isColEmpty = (cid) => this.cols[cid].rows.length == 1 && this.cols[cid].rows[0].process == null;
      const isRowEmpty = (cid, rid) => this.cols[cid].rows[rid].process == null;
      const newEmpty = () => ({process: null, processData: {identifier: null, inPorts: [], outPorts: [], data: null}})
      //cols
      for(let i = 0; i < this.cols.length; i++){
        if(!isColEmpty(i)){
          //left
          if(i - 1 < 0 || !isColEmpty(i - 1)){ this.cols.splice(i++, 0, {rows: [newEmpty()]}); }
          let rempty = false;
          //rows
          for(let ii = 0; ii < this.cols[i].rows.length; ii++){
            if(!isRowEmpty(i, ii)){
              //up
              if(ii - 1 < 0 || !isRowEmpty(i, ii - 1)){ this.cols[i].rows.splice(ii++, 0 , newEmpty()); }
              //down
              if(ii + 1 >= this.cols[i].rows.length || !isRowEmpty(i, ii + 1)){ this.cols[i].rows.splice(++ii, 0 , newEmpty()); }
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
          if(i + 1 >= this.cols.length || !isColEmpty(i + 1)){ this.cols.splice(++i, 0, {rows: [newEmpty()]}); }
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
      this.nullProcessRows[index].process = this.currentArrowData.currentArrow;
      this.nullProcessRows[index].processData.identifier = this.newIdentifier();
      this.nullProcessRows[index].processData.inPorts = [];
      this.nullProcessRows[index].processData.outPorts = [];
      this.nullProcessRows[index].processData.data =  { ...this.nullProcessRows[index].process.data };
      this.processRows.set(this.nullProcessRows[index].processData.identifier, this.nullProcessRows[index]);
      this.fixCols();
      Vue.nextTick(this.updatePositions)
    },
    onConnect(inIdentifier, inPortIndex, outIdentifier, outPortIndex){
      let curr = this.processRows.get(inIdentifier).processData.inPorts[inPortIndex]
      if(curr){
        this.processRows.get(curr.identifier).processData.outPorts[curr.portIndex] = null
      }
      this.processRows.get(outIdentifier).processData.outPorts[outPortIndex] = { identifier: inIdentifier, portIndex: inPortIndex}
      this.processRows.get(inIdentifier).processData.inPorts[inPortIndex] = { identifier: outIdentifier, portIndex: outPortIndex}
    },
    onContextMenu(pdata){
      let id = pdata.processData.identifier;
      if(id == this.specialRows.input.processData.identifier || id == this.specialRows.output.processData.identifier)
        return;
      this.processRows.get(id).processData.inPorts.forEach(x => {
        if(x) this.processRows.get(x.identifier).processData.outPorts[x.portIndex] = null
      })
      this.processRows.get(id).processData.outPorts.forEach(x => {
        if(x) this.processRows.get(x.identifier).processData.inPorts[x.portIndex] = null
      })
      this.processRows.get(id).process = null;
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
        o.processData.outPorts[i] = null
        this.processRows.get(pdata.processData.identifier).processData.inPorts[portIndex] = null
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
    }
  },
  mounted(){
    this.cols = this.startCols
    Vue.nextTick(() => {
      let s = this.$refs.grid.getspecialRowPoints();
      this.specialRows.input = {process: getInputProcess(), point: s.input, processData: {identifier: this.newIdentifier(), inPorts: [], outPorts: [], data: null}}
      this.specialRows.output = {process: getOutputProcess(), point: s.output, processData: {identifier: this.newIdentifier(), inPorts: [], outPorts: [], data: null}}
      this.processRows.set(this.specialRows.input.processData.identifier, this.specialRows.input);
      this.processRows.set(this.specialRows.output.processData.identifier, this.specialRows.output);
    })
  },
  computed: {
    connectors(){
      return Array.from(this.processRows).flatMap((x) => 
        x[1].processData.inPorts.filter(y => y).map(y => (
          { inIdentifier: x[1].processData.identifier, 
            inPortIndex: x[1].processData.inPorts.indexOf(y),
            outIdentifier: y.identifier,
            outPortIndex: y.portIndex,
          })
        )
      )
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
          <connector v-for="c in connectors" :inPort="processRows.get(c.inIdentifier).nodeContainer.inPorts[c.inPortIndex]" 
            :outPort="processRows.get(c.outIdentifier).nodeContainer.outPorts[c.outPortIndex]"></connector>
          <dragged-connector v-if="currentConnectorData.isDragging" ref="draggedConnector" :processRows="processRows" @onconnect="onConnect"></dragged-connector>
        </g>
      </g>
    </svg>
    <dragged-arrow :points="nullProcessRows" v-if="currentArrowData.isDraggingArrow" @onsnap="onSnap"></dragged-arrow>
  </div>
  `
}