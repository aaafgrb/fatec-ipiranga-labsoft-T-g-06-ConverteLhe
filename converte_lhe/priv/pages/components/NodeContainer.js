import Port from "./Port.js"

export default {
  components: {
    Port
  },
  data: () => ({
    position: {x: 0, y: 0},
    inPorts: [],
    outPorts: []
  }),
  inject: ['currentMenuData'],
  expose: ['update', 'position', 'inPorts', 'outPorts'],
  emits: ['oncontextmenu', 'startdragging'],
  props: {
    pdata: {
      type: Object,
      required: true
    },
    identifier: {
      type: Number,
      required: true
    }
  },
  mounted(){
    this.update()
  },
  updated(){
    this.pdata.nodeContainer = this
  },
  computed: {
  },
  methods: {
    update(){
      let {width, height} = this.$refs.container.getBoundingClientRect();
      this.position = { x: this.pdata.point.offsetLeft - width / 2 
                      , y: this.pdata.point.offsetTop - height / 2}
    },
    getPortHeight(id, total){
      let gap = total <= 1 ? 0 : 50 / (total - 1);
      return 50 + id * gap;
    },
    onMouseDown(event){
      if(event.button != 0) { return }
      if(this.currentMenuData.isOpen){
        this.currentMenuData.isOpen = false;
        if(this.currentMenuData.process != this.pdata){
          Vue.nextTick(() => {
            this.currentMenuData.process = this.pdata;
            this.currentMenuData.isOpen = true;
          })
        }
      }else{
        this.currentMenuData.process = this.pdata;
        this.currentMenuData.isOpen = true;
      }
    },
    oncontextmenu(){
      if(this.currentMenuData.isOpen && this.currentMenuData.process == this.pdata)
        this.currentMenuData.isOpen = false;
      this.$emit('oncontextmenu', this.pdata)
    },
    startDragging(isInput, portIndex){
      this.$emit('startdragging', this.pdata, isInput ? this.$refs.inPorts[portIndex] : this.$refs.outPorts[portIndex], portIndex)
    }
  },
  template: `
  <g class="node-container"
    @contextmenu.prevent="oncontextmenu"
    
    ref="container"
    :transform="\`translate(\${position.x}, \${position.y})\`"
  >
    <rect class="node-background" width="204" height="128" x="0" y="0" rx="6" ry="6" />
    <g class="node-header" :style="{fill: \`\${pdata.process.color}\`}" @mousedown="onMouseDown">
      <rect class="header-round-rect" width="200" height="40" x="2" y="2" rx="4" ry="4" />
      <rect class="header-rect" width="200" height="36" x="2" y="6" />
      <text class="header-title" x="102" y="30">{{pdata.process.label}}</text>
    </g>
    <g class="node-content">
      <rect class="content-round-rect" width="200" height="82" x="2" y="44" rx="4" ry="4" />
      <rect class="content-rect" width="200" height="77" x="2" y="44" @mousedown="onMouseDown" />
      <g class="inputs">
        <port v-for="(d, i) in pdata.process.inPorts" :isInput="true" :data="d" :index="i" 
          :top="getPortHeight(i, pdata.process.inPorts.length)" ref="inPorts" @startdragging="startDragging"></port>
      </g>
      <g class="outputs">
        <port v-for="(d, i) in pdata.process.outPorts" :isInput="false" :data="d" :index="i" 
          :top="getPortHeight(i, pdata.process.outPorts.length)" ref="outPorts" @startdragging="startDragging"></port>
      </g>
    </g>
  </g> 
  `
}