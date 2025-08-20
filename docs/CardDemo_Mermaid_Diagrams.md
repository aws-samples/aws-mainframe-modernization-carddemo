# CardDemo Mermaid Diagrams

**Application:** AWS.M2.CARDDEMO  
**Purpose:** Corrected Mermaid diagrams for CardDemo application flow

---

## 1. Application Entry Point Flow

```mermaid
flowchart TD
    A[User Access] --> B[CC00 Transaction]
    B --> C[COSGN00C - Signon Screen]
    C --> D{User Authentication}
    D -->|Valid Admin| E[CA00 - Admin Menu]
    D -->|Valid User| F[CM00 - Main Menu]
    D -->|Invalid| C
    E --> G[User Management Functions]
    F --> H[Business Functions]

    style A fill:#e1f5fe
    style B fill:#fff3e0
    style C fill:#f3e5f5
    style D fill:#e8f5e8
    style E fill:#fff8e1
    style F fill:#fff8e1
```

---

## 2. Main Application Architecture

```mermaid
flowchart TD
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

    style A fill:#e3f2fd
    style B fill:#f3e5f5
    style C fill:#fff3e0
    style D fill:#e8f5e8
    style E fill:#fce4ec
```

---

## 3. User Flow Sequence

```mermaid
flowchart LR
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

    style A fill:#e8f5e8
    style B fill:#f3e5f5
    style C fill:#fff3e0
    style D fill:#e1f5fe
    style E fill:#e1f5fe
    style U fill:#ffebee
```

---

## 4. Data Flow Architecture

```mermaid
flowchart TD
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

    style A fill:#e3f2fd
    style B fill:#f3e5f5
    style C fill:#fff3e0
    style D fill:#e8f5e8
    style E fill:#fce4ec
```

---

## 5. Transaction Processing Flow

```mermaid
flowchart TD
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

    style A fill:#e8f5e8
    style B fill:#fff3e0
    style C fill:#f3e5f5
    style D fill:#ffebee
    style E fill:#e1f5fe
    style L fill:#e8f5e8
    style N fill:#e8f5e8
```

---

## 6. Security and Authentication Flow

```mermaid
flowchart TD
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

    style A fill:#e8f5e8
    style C fill:#f3e5f5
    style F fill:#fff3e0
    style H fill:#e1f5fe
    style M fill:#fff3e0
    style P fill:#e1f5fe
    style Q fill:#e1f5fe
```

---

## 7. Menu Navigation Flow

```mermaid
flowchart TD
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

    style A fill:#e3f2fd
    style B fill:#fff3e0
    style C fill:#f3e5f5
    style D fill:#fff3e0
    style H fill:#e8f5e8
    style K fill:#e1f5fe
```

---

## 8. VSAM Data Access Flow

```mermaid
flowchart TD
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

    style A fill:#e3f2fd
    style B fill:#f3e5f5
    style C fill:#fff3e0
    style D fill:#e8f5e8
    style V fill:#e1f5fe
    style W fill:#ffebee
```

---

## Common Mermaid Syntax Issues and Solutions

### Issue 1: Graph Direction

**Problem:** `graph TD` syntax error
**Solution:** Use `flowchart TD` for top-down flowcharts

### Issue 2: Node References

**Problem:** Undefined node references
**Solution:** Ensure all nodes are properly defined before referencing

### Issue 3: Styling

**Problem:** Invalid style syntax
**Solution:** Use proper Mermaid styling syntax with `style` keyword

### Issue 4: Comments

**Problem:** Comment syntax not recognized
**Solution:** Use `%%` for single-line comments in Mermaid

### Issue 5: Subgraphs

**Problem:** Complex grouping issues
**Solution:** Use proper subgraph syntax with `subgraph` keyword

---

## Usage Notes

1. **Copy and paste** these diagrams into any Mermaid-compatible viewer
2. **Modify colors** by changing the style definitions
3. **Add new nodes** by following the existing pattern
4. **Update flows** by modifying the arrow connections
5. **Test syntax** in a Mermaid live editor before deployment

These diagrams provide a comprehensive view of the CardDemo application architecture and can be used for:

- **System documentation**
- **Migration planning**
- **Training materials**
- **Architecture reviews**
- **Development reference**
