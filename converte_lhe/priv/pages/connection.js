import ArrowSection from './components/ArrowSection.js'
import CompositionSection from './components/CompositionSection.js'
import DataSection from './components/DataSection.js'
import NodeMenu from './components/NodeMenu.js'
import { throttle } from './Util.js'

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
    currentMenuData: { isOpen: false, process: null},
    identifier: 0,
    ticking: false,
  }),
  provide(){
    return {
      mouseSvgPos: this.mouseSvgPos,
      mousePagePos: this.mousePagePos,
      currentArrowData: this.currentArrowData,
      currentConnectorData: this.currentConnectorData,
      currentMenuData: this.currentMenuData,
      newIdentifier: () => this.identifier++,
      getComposition: this.getComposition
    }
  },
  mounted(){
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
      return this.$refs.comp.getComposition()
    }
  },
  template: `
  <div class="row" id="comp-area" style="resize: vertical; overflow: auto; height: 50vh; justify-content: center;" ref="compArea">
    <div class="gx-0" style="width: 100%;">
      <composition-section ref="comp"></composition-section>
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


