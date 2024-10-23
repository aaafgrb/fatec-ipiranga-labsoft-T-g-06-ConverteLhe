import Connector from "./Connector.js";
import {isCollidingRect} from "../Util.js"

export default {
  components: {
    Connector
  },
  props: {
    processRows: {
      type: Object,
      required: true
    }
  },
  watch: {
  },
  expose: ['update'],
  emits: ['onconnect'],
  computed: {
    mousePort(){
      return {position: { x: this.mouseSvgPos.x, y: this.mouseSvgPos.y} };
    },
    inPort(){
      return this.currentConnectorData.fixedPort.isInput ? this.currentConnectorData.fixedPort : this.mousePort;
    },
    outPort(){
      return this.currentConnectorData.fixedPort.isInput ? this.mousePort : this.currentConnectorData.fixedPort;
    },
  },
  inject: ['mouseSvgPos', 'currentConnectorData'],
  data: () => ({
  }),
  methods: {
    onMouseUp(event){
      let isInput = !this.currentConnectorData.fixedPort.isInput
      let handle = isInput ? this.$refs.connector.$refs.inputHandle : this.$refs.connector.$refs.outputHandle

      let hit = null;
      let ii = 0;
      this.processRows.forEach((value, key) => {
        if(isCollidingRect(handle, value.nodeContainer.$refs.container)){
          if(isInput){
            for(ii = 0; ii < value.nodeContainer.inPorts.length; ii++){
              if(isCollidingRect(handle, value.nodeContainer.inPorts[ii].$refs.portCircle)){
                hit = key;
                break;
              }
            }
          }else{
            for(ii = 0; ii < value.nodeContainer.outPorts.length; ii++){
              if(isCollidingRect(handle, value.nodeContainer.outPorts[ii].$refs.portCircle)){
                hit = key;
                break;
              }
            }
          }
        }
      });

      if(hit != null) {
        if(this.currentConnectorData.identifier != hit){
          if(isInput) 
            this.$emit('onconnect', hit, ii, this.currentConnectorData.identifier, this.currentConnectorData.portIndex)
          else 
            this.$emit('onconnect', this.currentConnectorData.identifier, this.currentConnectorData.portIndex, hit, ii)
        }
      }
      this.currentConnectorData.isDragging = false;
    },
  },
  mounted(){
    this.currentConnectorData.callback = this.onMouseUp;
  },
  watch: {
  },
  template: `
  <connector ref="connector" 
    :inPort="inPort" 
    :outPort="outPort"
  ></connector>
  `
}

  