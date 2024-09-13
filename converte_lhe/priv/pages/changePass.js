function changePass(){
    const pass = document.getElementById("passTxt");

    //todo: make password checking

    const feedback = document.getElementById("feedbackLbl")
  
    let params = new URLSearchParams(document.location.search);
    let key = params.get("k");

    if(key == null){
        feedback.innerHTML = "password change failed: invalid reset key";
        return;
    }

    const obj = {
        req: "changePass",
        key: key,
        pass: pass.value,
    };
    
    const response = fetch("./auseronn", {
      method: 'POST',
      body: JSON.stringify(obj),
    });
  
    feedback.innerHTML = "fetching...";
  
    response.then(async (res) => {
      let j = await res.json();
      feedback.innerHTML = j.error ? "error: " + j.error : "fetch resulted in: " + j.value;
    })
  }