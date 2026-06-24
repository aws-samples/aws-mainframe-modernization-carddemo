# ADR 0005 — Templating and UI: BMS screen replacement

**Status:** Accepted (RAU-36)

## Context

Each CICS transaction in CardDemo has a BMS map that defines a terminal screen layout
(field positions, attributes, colours). The Go port must replace these with web pages.

## Decision

**`html/template` + minimal CSS, one HTML file per BMS map. No SPA.**

- Each BMS map becomes one Go `html/template` template under `internal/web/<domain>/templates/`.
- Templates are embedded into the binary via `embed.FS`.
- Styling is minimal, utility-CSS-based; no JavaScript framework.
- Forms POST back to the same or a redirect URL; no client-side state.

## BMS map → template mapping

| BMS map     | Template path                    | Owner  |
|-------------|----------------------------------|--------|
| COSGN00.bms | web/auth/templates/signin.html   | RAU-39 ✓ |
| COADM01.bms | web/admin/templates/admin.html   | RAU-39 ✓ |
| COMEN01.bms | web/menu/templates/menu.html     | RAU-43 |
| COACTVW.bms | web/account/templates/view.html  | RAU-38 |
| COACTUP.bms | web/account/templates/update.html| RAU-38 |
| COCRDSL.bms | web/card/templates/select.html   | RAU-40 |
| COCRDLI.bms | web/card/templates/list.html     | RAU-40 |
| COCRDUP.bms | web/card/templates/update.html   | RAU-40 |
| COTRN00.bms | web/txn/templates/list.html      | RAU-41 |
| COTRN01.bms | web/txn/templates/add.html       | RAU-41 |
| COTRN02.bms | web/txn/templates/view.html      | RAU-41 |
| COBIL00.bms | web/billing/templates/pay.html   | RAU-42 |
| CORPT00.bms | web/report/templates/report.html | RAU-45 |
| COUSR00.bms | web/user/templates/list.html     | RAU-39 ✓ |
| COUSR01.bms | web/user/templates/add.html      | RAU-39 ✓ |
| COUSR02.bms | web/user/templates/update.html   | RAU-39 ✓ |
| COUSR03.bms | web/user/templates/delete.html   | RAU-39 ✓ |

## Consequences

- No JavaScript build step; the binary is self-contained.
- Screen-flow (which CICS SEND MAP to issue) is replaced by HTTP redirects.
- Auto-escaping in `html/template` prevents XSS by default.
