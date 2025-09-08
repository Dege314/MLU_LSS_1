install.packages("DiagrammeR")
library(DiagrammeR)
workflow <- "
%%{init: {'theme': 'base', 'themeVariables': { 'fontSize': '10px', 'fontFamily': 'arial' }, 'flowchart': { 'nodeSpacing': 7,'rankSpacing': 20 }}}%%
flowchart TD
    %% Konfiguration für gerade/eckige Linien
    linkStyle default orthogonal

    %% S1-Subgraph
    subgraph Sentinel1[" "]
        direction TB
        Title["Sentinel-1 Level-1 GRD"]
        Title --> AOF
        AOF["Apply-Orbit-File"] --> TNR["Thermal-Noise-Removal"]
        TNR --> BNR["Border-Noise-Removal"]
        BNR --> Cal["Calibration-Sigma-Nought"]
        Cal --> SPK["Speckle-Filter"]
        SPK --> TCR["Terrain-Correction"]
        TCR --> LDB["Linear-DB"]
        LDB --> 1SUB["Subset"]
        1SUB --> 1MSC["Mosaic"]
        1MSC --> Mean["Averaging of two <br> closest products"]
        1SUB --> Mean
    end

    %% Sentinel-2 Subgraph
    subgraph Sentinel2[" "]
        direction TB
        Title2["Sentinel-2 Level 2A"]
        Title2 --> 2SUB
        2SUB["Subset"] --> CLM["Cloud-Masking"]
        CLM --> 2MSC["Mosaic"]
    end

    %% Analyse Subgraph
    subgraph Classification[" "]
        direction TB
       RF["Random-Forest"]
    end

    %% Verbindungen zwischen Subgraphs
        Mean --> RF
        2MSC --> RF

    %% Stil- und Layoutoptimierungen
    classDef box fill:#f9f9f9,stroke:#333,stroke-width:1px,font-size:10px
    classDef subgraph_style fill:#eeeeee,stroke:#666,stroke-width:2px,font-size:15px, padding:10
    class AOF,TNR,BNR,Cal,SPK,TCR,LDB,1SUB,1MSC,Mean,2SUB,CLM,RF,1MSC,2MSC,App box
    class Sentinel1,Sentinel2,Classification,Title,Title2,RF subgraph_style"

mermaid(workflow)










