### Dokumentation of project "-" code and data

### Workflow
```mermaid
%%{init: {'theme': 'base', 'themeVariables': { 'fontSize': '10px', 'fontFamily': 'arial' }}}%%
graph TD
    %% Konfiguration für gerade/eckige Linien
    linkStyle default orthogonal

    %% Historische Daten Subgraph
    subgraph historisch["Historisch"]
        direction LR
        DWD["DWD 1961-2020"] --> F1["Filter<br>(keine Datenlücken)"]
        F1 --> P1["Räumlichen Bezug<br>erstellen"]
    end

    %% Projektionen Subgraph
    subgraph projektionen["Projektionen"]
        direction TB
        REMO["REMO2009 2011-2060"] --> N2R["NetCDF → Raster<br>• Flip<br>• Projizieren<br>• Maskieren"]
        N2R --> PEX["Punkte extrahieren"]
        PEX --> DIF["Differenz Historisch &<br>Projektion 2011-2020"]
        DIF --> PRJ["Projektion anpassen"]
    end

    %% Analyse Subgraph
    subgraph analyse["Analyse"]
        direction LR
        KR["KGK"] --> IN["Interpolation"]
        PC["PCA"] --> CL["Cluster"]
        CL --> IN
        IN --> Map["• Karten"]
        IN --> CV["• Kreuzvalidierung"]
        Map --> App["• Shiny App"]
        CV --> App
    end

    %% Verbindungen zwischen Subgraphs
    P1 --> PEX
    P1 --> KR
    P1 --> PC
    PRJ --> KR
    PRJ --> PC

    %% Stil- und Layoutoptimierungen
    classDef box fill:#f9f9f9,stroke:#333,stroke-width:1px,font-size:20px
    classDef subgraph_style fill:#eeeeee,stroke:#666,stroke-width:2px,font-size:35px
    class DWD,REMO,F1,N2R,PEX,DIF,PRJ,P1,KR,PC,CL,IN,Map,CV,App box
    class historisch,projektionen,analyse subgraph_style
```
