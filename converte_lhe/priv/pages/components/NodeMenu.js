import MenuGroup from "./MenuGroup.js"
export default {
  components: {
    MenuGroup
  },
  props: {
  },
  inject: ['currentMenuData'],
  data: () => ({
    data: null,
    point: null
  }),
  beforeMount(){
    this.data = this.currentMenuData.process;
    this.point = this.data.point
    
  },
  mounted(){
    if(this.currentMenuData.process.process.menu.length == 0){
      this.currentMenuData.isOpen = false; 
    }
  },
  beforeUnmount(){

  },
  template: `
  <Teleport defer :to="point">
    <div class="menu-container" ref="container">
      <menu-group v-for="(g, i) in data.process.menu" :data="g" :curr="currentMenuData.process.processData.data" :index="i"></menu-group>
    </div>
  </Teleport>
  `
}