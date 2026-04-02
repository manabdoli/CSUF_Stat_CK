document.addEventListener("DOMContentLoaded", function () {
  document.querySelectorAll("h3").forEach(h3 => {
    h3.classList.add("h3-collapsible");
    h3.addEventListener("click", () => {
      h3.classList.toggle("active");
      let next = h3.nextElementSibling;
      if (next) {
        next.style.display = next.style.display === "block" ? "none" : "block";
      }
    });
  });
});
