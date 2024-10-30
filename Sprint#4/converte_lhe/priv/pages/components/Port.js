
export default {
  components: {
  },
  emits: ['startdragging'],
  expose: ['position', 'isInput', 'connector'],
  props: {
    data: {
      type: Object,
      required: true
    },
    top: {
      type: Number,
      default: 50
    },
    isInput: {
      type: Boolean,
      required: true
    },
    index: {
      type: Number,
      required: true
    },
    startConnector: {
      type: Object,
      required: false
    }
  },
  methods: {
    onMouseDown(event){
      this.$emit('startdragging', this.isInput, this.index)
    },
  },
  computed: {
    det(){
      return this.isInput ? 
        {class: "input-field", cx: 15, x: 28} : 
        {class: "output-field", cx: 189, x: 176}
    },
    position(){
      return {x: this.$parent.position.x + this.det.cx, y: this.$parent.position.y + this.top + 10}
    }
  },
  data: () => ({
    connector: null,
  }),
  mounted(){
    this.connector = this.startConnector;
    if(this.isInput){ this.$parent.inPorts[this.index] = this }
    else{this.$parent.outPorts[this.index] = this}
  },
  template: `
  <g :class="det.class" :transform="\`translate(0, \${top})\`">
    <g class="port" @mousedown="onMouseDown" ref="portCircle">
      <circle class="port-outer" :cx="det.cx" cy="10" r="7.5" />
      <circle class="port-inner" :cx="det.cx" cy="10" r="5" />
      <circle class="port-scrim" :cx="det.cx" cy="10" r="7.5" />
    </g>
    <text class="port-label" :x="det.x" y="10">{{data.label}}</text>
    <text class="port-subtitle" :x="det.x" y="19">{{data.subtitle}}</text>
  </g>
  `
}