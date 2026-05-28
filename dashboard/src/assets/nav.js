const TOOL_SUITE_ROOT = "/nba-research-project/";

const pages = [
  { href: "index.html", label: "Welcome" },
  { href: "findings.html", label: "Key Findings" },
  { href: "overview.html", label: "Overview" },
  { href: "team.html", label: "Team" },
  { href: "explorer.html", label: "Explorer" },
  { href: "sandbox.html", label: "Sandbox" },
];

function renderNav() {
  const host = document.getElementById("tmg-nav-host");
  if (!host) {
    return;
  }

  const links = pages
    .map((page) => `<a href="${page.href}">${page.label}</a>`)
    .join("");

  host.innerHTML = `
    <nav class="tmg-nav" aria-label="TMG Tool Suite Navigation">
      <a class="brand" href="${TOOL_SUITE_ROOT}">TMG Tool Suite | NBA Analysis</a>
      <div class="dropdown">
        <button class="dropbtn" type="button">Pages</button>
        <div class="dropdown-content">
          ${links}
        </div>
      </div>
    </nav>
  `;
}

document.addEventListener("DOMContentLoaded", renderNav);