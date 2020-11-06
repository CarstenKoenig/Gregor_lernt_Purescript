# Gregor_lernt_Purescript

Material und Einstiegsprojekte für [Functional Programming Next Level mit PureScript](https://www.meetup.com/de-DE/My-Coding-Zone/events/274350964/)
mit **Gregor Biswanger**

## Loslegen

- Clonen `git clone https://github.com/CarstenKoenig/Gregor_lernt_Purescript.git`
- (optional) gewünschten Branch auschecken `git checkout SnakeStart`
- **PureScript**, **Parcel** und **Spago** (Build-Tool für PureScript) per *npm* installieren lassen `npm install`
- starten `npm start` (sollte erst Packages herunterladen + kompilieren - etwas gedult bitte)
- [localhost:1234](http://localhost:1234) im Browser aufrufen

## Coden

ich verwende **VS.code** mit der [PureScript IDE](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript) Erweiterung.

Die sollte *Syntax-Highlighting* und Error-Reporting, Tooltips, ... installieren.

### übliche Problemchen

**Wichtig:** diese Erweiterung arbeitet mit dem kompilierten Modulen im `output` Verzeichnis - ihr sollet also erstmal `npx spago build` aufrufen, damit das angelegt/gefüllt wird.
(`npm start` wie oben macht das jeweils vor dem Starten von *parcel* mit).

Danach übernimmt das **VS.code** normalerweise beim Speichern der Datei für euch - was nett ist, weil **Parcel** die Common-JS Module dort verwenden kann und ihr sowas
wie Hot-Reloading automatisch bekommt (naja im Moment ist der Zustand der App immer weg ... da gibt es Wege - siehe Spago Doku - aber für uns reicht das denke ich heute)

Beim *Refactorieren* kann es immer sein, dass im Output-Modul noch alte Module rumliegen - da **VS.code** damit arbeitet bekommt ihr dann falsche/komische Vorschläge.
Meines Wissens gibt es leider im Moment keinen einfachen Weg das aufzuräumen - ich lösche dann einfach das Verzeichnis (`rm -rf output`) und mache ein neues `npx spago build`.

## Snake Game

Im Branch [SnakeStart](https://github.com/CarstenKoenig/Gregor_lernt_Purescript/tree/SnakeStart) findet ihr einen Startpunkt für eigenes Coding.

Das Fertige Spiel ist unter [https://github.com/CarstenKoenig/Gregor_lernt_Purescript/tree/Snake] zu finden.
