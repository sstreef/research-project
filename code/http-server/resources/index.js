window.onload = function() {
    const display = document.getElementById("fetch__display")


        document
            .getElementById("fetch__json_button")
            .addEventListener("click", async () => 
                await fetch("/data.json")
                    .then(resp => resp.json())
                    .then(data => display.innerHTML = JSON.stringify(data, null, 3))
            )
    
}