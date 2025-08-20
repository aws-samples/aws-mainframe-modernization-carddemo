# CardDemo Simple Mermaid Diagrams

**Application:** AWS.M2.CARDDEMO  
**Purpose:** Simplified Mermaid diagrams for maximum compatibility

---

## 1. Basic Application Flow

```mermaid
graph TD
    A[User Access] --> B[CC00 Transaction]
    B --> C[COSGN00C - Signon Screen]
    C --> D{User Authentication}
    D -->|Valid Admin| E[CA00 - Admin Menu]
    D -->|Valid User| F[CM00 - Main Menu]
    D -->|Invalid| C
    E --> G[User Management Functions]
    F --> H[Business Functions]
```

---

## 2. User Navigation Flow

```mermaid
graph LR
    A[Start] --> B[CC00 - Signon]
    B --> C{User Type?}
    C -->|Admin| D[CA00 - Admin Menu]
    C -->|User| E[CM00 - Main Menu]

    D --> F[CU00 - User List]
    D --> G[CU01 - User Add]
    D --> H[CU02 - User Update]
    D --> I[CU03 - User Delete]

    E --> J[CAVW - Account View]
    E --> K[CAUP - Account Update]
    E --> L[CCLI - Card List]
    E --> M[CCDL - Card View]
    E --> N[CCUP - Card Update]
    E --> O[CT00 - Transaction List]
    E --> P[CT01 - Transaction View]
    E --> Q[CT02 - Transaction Add]
    E --> R[CR00 - Reports]
    E --> S[CB00 - Bill Payment]

    F --> T[Return to Menu]
    G --> T
    H --> T
    I --> T
    J --> T
    K --> T
    L --> T
    M --> T
    N --> T
    O --> T
    P --> T
    Q --> T
    R --> T
    S --> T

    T --> U[PF3 - Logout]
    U --> B
```

---

## 3. Data Flow Architecture

```mermaid
graph TD
    A[User Interface] --> B[CICS Transaction Manager]
    B --> C[COBOL Programs]
    C --> D[VSAM Data Access]
    D --> E[VSAM Files]

    A --> A1[BMS Maps]
    A --> A2[Screen Layouts]

    C --> C1[Business Logic]
    C --> C2[Data Validation]
    C --> C3[Error Handling]

    E --> E1[ACCTDAT - Account Data]
    E --> E2[CARDDAT - Card Data]
    E --> E3[CUSTDAT - Customer Data]
    E --> E4[TRANSACT - Transaction Data]
    E --> E5[USRSEC - Security Data]
    E --> E6[CCXREF - Cross Reference]

    C1 --> D
    C2 --> D
    C3 --> D
```

---

## 4. Transaction Processing Flow

```mermaid
graph TD
    A[Transaction Request] --> B[Input Validation]
    B --> C{Valid Input?}
    C -->|No| D[Error Message]
    C -->|Yes| E[Data Processing]

    E --> F[VSAM File Access]
    F --> G{Operation Type?}

    G -->|Read| H[Retrieve Data]
    G -->|Write| I[Update Data]
    G -->|Add| J[Insert Data]
    G -->|Delete| K[Remove Data]

    H --> L[Format Response]
    I --> L
    J --> L
    K --> L

    D --> M[Display Error]
    L --> N[Display Results]

    M --> O[Return to Menu]
    N --> O
```

---

## 5. Security and Authentication Flow

```mermaid
graph TD
    A[User Login] --> B[Enter CC00]
    B --> C[COSGN00C - Signon Screen]
    C --> D[User ID & Password Input]
    D --> E[Validate Input Fields]
    E --> F{Fields Valid?}

    F -->|No| G[Display Error]
    F -->|Yes| H[Read USRSEC File]

    H --> I{User Found?}
    I -->|No| J[User Not Found Error]
    I -->|Yes| K{Password Match?}

    K -->|No| L[Wrong Password Error]
    K -->|Yes| M{User Type?}

    M -->|Admin| N[Route to Admin Menu]
    M -->|User| O[Route to User Menu]

    G --> C
    J --> C
    L --> C

    N --> P[CA00 - Admin Functions]
    O --> Q[CM00 - User Functions]
```

---

## 6. Menu Navigation Flow

```mermaid
graph TD
    A[Menu Display] --> B[User Selection]
    B --> C[Validate Option]
    C --> D{Valid Option?}

    D -->|No| E[Display Error]
    D -->|Yes| F{User Type Check}

    F -->|Admin Only| G{User is Admin?}
    F -->|All Users| H[Execute Function]

    G -->|No| I[Access Denied]
    G -->|Yes| H

    E --> J[Return to Menu]
    I --> J
    H --> K[Program Execution]
    K --> L[Return to Menu]

    J --> A
    L --> A
```

---

## 7. VSAM Data Access Flow

```mermaid
graph TD
    A[COBOL Program] --> B[EXEC CICS READ/WRITE]
    B --> C[CICS File Control]
    C --> D[VSAM Dataset Access]
    D --> E{Operation Type?}

    E -->|READ| F[Retrieve Record]
    E -->|WRITE| G[Update Record]
    E -->|ADD| H[Insert Record]
    E -->|DELETE| I[Remove Record]

    F --> J{Record Found?}
    G --> K{Update Successful?}
    H --> L{Insert Successful?}
    I --> M{Delete Successful?}

    J -->|Yes| N[Return Data]
    J -->|No| O[File Not Found Error]

    K -->|Yes| P[Update Confirmed]
    K -->|No| Q[Update Failed Error]

    L -->|Yes| R[Insert Confirmed]
    L -->|No| S[Insert Failed Error]

    M -->|Yes| T[Delete Confirmed]
    M -->|No| U[Delete Failed Error]

    N --> V[Process Data]
    P --> V
    R --> V
    T --> V

    O --> W[Error Handling]
    Q --> W
    S --> W
    U --> W

    V --> X[Return to Program]
    W --> X
```

---

## 8. Application Architecture Overview

```mermaid
graph TD
    A[CardDemo Application] --> B[Authentication Layer]
    A --> C[Menu Layer]
    A --> D[Business Logic Layer]
    A --> E[Data Access Layer]

    B --> B1[COSGN00C - Signon]
    C --> C1[COADM01C - Admin Menu]
    C --> C2[COMEN01C - User Menu]

    D --> D1[Account Management]
    D --> D2[Credit Card Management]
    D --> D3[Transaction Management]
    D --> D4[User Management]
    D --> D5[Reporting & Billing]

    E --> E1[VSAM Files]
    E --> E2[USRSEC - Security]

    D1 --> E1
    D2 --> E1
    D3 --> E1
    D4 --> E2
    D5 --> E1
```

---

## Troubleshooting Tips

### If you still get errors:

1. **Try this minimal test diagram:**

```mermaid
graph TD
    A[Start] --> B[End]
```

2. **Check your Mermaid version:**

   - Some older versions don't support `flowchart`
   - Use `graph` instead of `flowchart`

3. **Remove styling:**

   - The `style` commands might not be supported in all versions
   - Try without styling first

4. **Use a different viewer:**

   - [Mermaid Live Editor](https://mermaid.live/)
   - [Mermaid.js.org](https://mermaid.js.org/demo/)
   - GitHub/GitLab markdown

5. **Check for special characters:**
   - Some viewers are sensitive to special characters in node names
   - Use simple text without brackets or special symbols

---

## Usage Notes

These simplified diagrams should work with:

- **GitHub/GitLab** markdown rendering
- **Mermaid Live Editor**
- **VS Code** with Mermaid extensions
- **Most Mermaid viewers**

The diagrams show the same CardDemo application flow but use basic syntax for maximum compatibility.
