async function sendRequest() {
    const form = document.getElementById("inForm");
    const formData  = new FormData(form);
      
    //todo: insert composition data here
    //formData.append("test", "dataT");
  
    const response = await fetch("./form", {
      method: 'POST',
      body: formData
    });

    //todo: show the results on the screen
    console.log(response);
  }