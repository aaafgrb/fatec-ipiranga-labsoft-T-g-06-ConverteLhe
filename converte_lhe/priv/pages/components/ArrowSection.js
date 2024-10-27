import { getArrowList } from "../functionList.js"

export default {
  components: {
  },
  props: {
  },
  emits: ['onstartdrag'],
  data: () => ({
    pileList: getArrowList(),
    startDrag(k, pile){
      this.$emit('onstartdrag', pile);
    },
  }),
  mounted(){
  },
  template: `
  <div class="row arrow-section justify-content-center" id="arrow-section">
    <div class="draggable-pile col col-3 m-2" 
      v-for="[k, pile] in pileList"
      :style="{backgroundColor: pile.color}"
      @mousedown="startDrag(k, pile)"
    >{{pile.label}}</div>
  </div>
  `
}