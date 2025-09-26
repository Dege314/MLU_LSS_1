### Dokumentation of project "-" code and data

## Workflow

```mermaid
%%{init: {'theme': 'base', 'themeVariables': { 'fontSize': '10px', 'fontFamily': 'arial' }, 'flowchart': { 'nodeSpacing': 7,'rankSpacing': 15 }}}%%
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
        Mean --> TP["calcuate texture parameters"]
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
        BE["VH & VV buffer value extraction"]
        PE["Pixel value etraction"]
        BE --> CD["CohensD difference analysis per variable"]
        PE --> CD
        CD --> BV["Choose best differencing <br> optical and SAR Variables"]
        BV --> RF1["Random-Forest of all possible <br> variable cominations"]
        RF1 --> SB["select best variable combinations"]
        SB --> Optical["Optical"]
        SB --> SAR["SAR"]
        SB --> Mixed["Mixed"]
        Optical --> RF2["Random-Forest classification for each time-point"]
        SAR --> RF2
        Mixed -->RF2

        
    end

    %% Verbindungen zwischen Subgraphs
        TP --> PE
        2MSC --> PE
        TP --> BE

    %% Stil- und Layoutoptimierungen
    classDef box fill:#f9f9f9,stroke:#333,stroke-width:1px,font-size:10px
    classDef subgraph_style fill:#eeeeee,stroke:#666,stroke-width:2px,font-size:15px, padding:10
    class AOF,TNR,BNR,Cal,SPK,TCR,LDB,1SUB,1MSC,Mean,TP,2SUB,CLM,BE,PE,CD,BV,RF1,RF2,SB,1MSC,2MSC,Optical,SAR,Mixed,App box
    class Sentinel1,Sentinel2,Classification,Title,Title2,RF subgraph_style
```
