const TOOL_SUITE_ROOT = "/nba-research-project/";

const pages = [
  { href: "index.html", label: "Welcome" },
  { href: "findings.html", label: "Key Findings" },
  { href: "overview.html", label: "Overview" },
  { href: "team.html", label: "Team" },
  { href: "explorer.html", label: "Explorer" },
  { href: "sandbox.html", label: "Sandbox" },
];

const toolSuiteRepos = [
  {
    href: "https://rmallorybpc.github.io/pdf-multi-agent-analysis/",
    label: "PDF Multi-Agent Analysis",
  },
  {
    href: "https://rmallorybpc.github.io/housing-market-intel/",
    label: "Housing Market Intel",
  },
  {
    href: "https://rmallorybpc.github.io/real-estate-report/",
    label: "Real Estate Report",
  },
  {
    href: "https://rmallorybpc.github.io/nflanalysis/dashboard/src/",
    label: "NFL Analysis",
  },
  {
    href: "https://rmallorybpc.github.io/nhl-free-agency-research/",
    label: "NHL Analysis",
  },
  {
    href: "https://rmallorybpc.github.io/recipes/",
    label: "Recipe Book",
  },
];

function renderNav() {
  const host = document.getElementById("tmg-nav-host");
  if (!host) {
    return;
  }

  const path = window.location.pathname;
  const currentPage = path.split("/").pop() || "index.html";

  const pageLinks = pages
    .map((page) => {
      const isCurrent = currentPage === page.href;
      const currentAttr = isCurrent ? ' aria-current="page"' : "";
      return `<a href="${page.href}"${currentAttr}>${page.label}</a>`;
    })
    .join("");

  const repoLinks = toolSuiteRepos
    .map(
      (repo) =>
        `<a href="${repo.href}" target="_blank" rel="noopener noreferrer" role="menuitem">${repo.label}</a>`
    )
    .join("");

  host.innerHTML = `
    <nav class="tmg-nav" role="banner" aria-label="TMG Tool Suite Navigation">
      <a class="tmg-logo" href="${TOOL_SUITE_ROOT}" aria-label="The Mallory Group">
        <span>TM</span>
        <div class="tmg-logo-divider" aria-hidden="true"></div>
        <span>G</span>
      </a>
      <div class="tmg-page-links" aria-label="Dashboard pages">
        ${pageLinks}
      </div>
      <div class="tmg-dropdown-wrap">
        <button class="tmg-dropdown-btn" id="tmgDdBtn" type="button" aria-haspopup="true" aria-expanded="false" aria-controls="tmgDdMenu">
          TMG Tool Suite
          <svg class="tmg-chevron" viewBox="0 0 12 12" fill="none" width="12" height="12" aria-hidden="true">
            <path d="M2 4l4 4 4-4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
          </svg>
        </button>
        <div class="tmg-dropdown-menu" id="tmgDdMenu" role="menu">
          ${repoLinks}
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