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
    .map((page) => `<a href="${page.href}" role="menuitem">${page.label}</a>`)
    .join("");

  host.innerHTML = `
    <nav class="tmg-nav" role="banner" aria-label="TMG Tool Suite Navigation">
      <a class="tmg-logo" href="${TOOL_SUITE_ROOT}" aria-label="The Mallory Group">
        <span>TM</span>
        <div class="tmg-logo-divider" aria-hidden="true"></div>
        <span>G</span>
      </a>
      <div class="tmg-dropdown-wrap">
        <button class="tmg-dropdown-btn" id="tmgDdBtn" type="button" aria-haspopup="true" aria-expanded="false" aria-controls="tmgDdMenu">
          TMG Tool Suite
          <svg class="tmg-chevron" viewBox="0 0 12 12" fill="none" width="12" height="12" aria-hidden="true">
            <path d="M2 4l4 4 4-4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
          </svg>
        </button>
        <div class="tmg-dropdown-menu" id="tmgDdMenu" role="menu">
          ${links}
        </div>
      </div>
    </nav>
  `;

  const btn = document.getElementById("tmgDdBtn");
  const menu = document.getElementById("tmgDdMenu");
  if (!btn || !menu) {
    return;
  }

  btn.addEventListener("click", function (event) {
    event.stopPropagation();
    const isOpen = menu.classList.toggle("open");
    btn.classList.toggle("open", isOpen);
    btn.setAttribute("aria-expanded", String(isOpen));
  });

  document.addEventListener("click", function () {
    menu.classList.remove("open");
    btn.classList.remove("open");
    btn.setAttribute("aria-expanded", "false");
  });

  document.addEventListener("keydown", function (event) {
    if (event.key === "Escape") {
      menu.classList.remove("open");
      btn.classList.remove("open");
      btn.setAttribute("aria-expanded", "false");
      btn.focus();
    }
  });
}

document.addEventListener("DOMContentLoaded", renderNav);