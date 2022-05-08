#!/usr/bin/env amm
import $ivy.`de.qwertyuiop::copret:0.0.1`
import de.qwertyuiop.copret._
import de.qwertyuiop.copret.syntax._
import TypedCommand.{interactive, shell => sh}, Format.figlet

import ammonite.ops.{given, *}


/* Configuration */

/* store paths for reuse.
 * most of the commands in this presentation will be git commands in a demo repository, so the path to that repo is set
 * as implicit (will be the working directory of executed commands) */
val imgs = pwd/"img"
given repoDir: Path= pwd/"demoRepo"
import de.qwertyuiop.copret.{given Theme}

/* You can define any variables and use then in your presentation.
 * The presentation is pure Scala code, you can use anything that Scala offers . */
val debug = true

/* image height is defined by the iterm2 image protocol as follows:
 *   N: N character cells.
 *   Npx: N pixels.
 *   N%: N percent of the session's width or height.
 *   auto: The image's inherent size will be used to determine an appropriate dimension.
 *
 * The `Image` slide element will default to 100% height and width (preserving aspect ratio). Here the maximum height
 * is calculated for slides with the default header (1 row status, 3 rows title font, 3 rows spacing)
 */ 
val imageHeight = (rows - 7).toString

/* Make sure, the demo repo is not there in the beginning. We'll create it in the presentation */
rm! repoDir

/* the basic building block of copret presentations is the various case classes of the `Slide` trait */
val titleSlide =
  Group( // `Group` combines several `Slide` objects into one
    Clear, // `Clear` clears the screen, so the next slide element starts at the top
    Image(imgs/"git-logo.png", width = "10%"), // `Image` currently requires a terminal supporting `imgcat`
    Paragraph( // `Paragraph` simply prints its text contents
      s"""${figlet("git", "roman").block.blue}
         |
         |${"Wie man es benutzt und was in .git eigentlich passiert".centered.blue}
         |
         |${"-- Alexander Gehrke".right(10)}
         |""".stripMargin)
      // Above you see some of the extension methods for strings, `block` for centering multiline text, `centered` for
      // single lines, `right(10)` for placing text 10 spaces away from the right border and `blue` for coloring.
      // also, `figlet` calls the external figlet command to render ASCII art text with the given font.
  )

/* copret templates are simply normal methods that return slides */
def chapter(title: String, subtitle: String): Group = chapter(title, "", subtitle)
def chapter(title1: String, title2: String, subtitle: String): Group = {
  val font = if((title1.length max title2.length) < columns / 10) "mono12" else "mono9"
  Group(Clear,
    header, // a built in template
    Paragraph(
      figlet(title1, font).block.green ++ figlet(title2, font).block.green ++ "\n" ++ subtitle.right(10).green
    )
  )
}


val parentSHApattern = raw"parent ([a-f0-9]{40})".r
val treeSHApattern = raw"tree ([a-f0-9]{40})".r
val blobSHApattern = raw"blob ([a-f0-9]{40})".r

/* With `prepare` you can define variables for a slide, that are computed when the presentation reaches the
 * slide using it. This is useful for running external commands when a specific slide is reached and using their
 * output (if you'd include them in the string interpolation directly, they'd run before starting the presentation).
 *
 * The code given to `prepare` is run exactly once *per slide using it*. See the usages of `gitHashes` below for
 * details. */
case class GitHashes(commit: String, tree: String, blob: String, parent: String, subtree: String)
val gitHashes = prepare {
    val commitSHA = %%%("git", "show-ref").substring(0,40)
    val commit = %%%("git", "cat-file", "-p", "HEAD")
    val treeSHA = treeSHApattern.findFirstMatchIn(commit).map(_.group(1)).getOrElse("")
    val parentSHA = parentSHApattern.findFirstMatchIn(commit).map(_.group(1)).getOrElse("")
    val tree = %%%("git", "cat-file", "-p", treeSHA)
    val blobSHA = blobSHApattern.findFirstMatchIn(tree).map(_.group(1)).getOrElse("")
    val subtreeSHA = treeSHApattern.findFirstMatchIn(tree).map(_.group(1)).getOrElse("")
    GitHashes(commitSHA, treeSHA, blobSHA, parentSHA, subtreeSHA)
  }

def gitStatus = TypedCommand("git", "status")
def gitCatFile(ref: String) = TypedCommand("git", "cat-file", "-p", ref)

def gitLogGraph(cutOff: Int = 0) = {
  val shownCmd = "git log --graph --oneline --all"
  val execCmd = shownCmd + " --decorate=short --color=always " + (if(cutOff > 0) "| head -n -" + cutOff else "")
  val typer = sh(execCmd) showing shownCmd
  if(cutOff > 0)
    Group(typer, "...".text)
  else
    typer
}

/* A `Presentation` object can include a map of metadata. Which keys are used, depends on your used templates.
 * To use them in a slide yourself (or for creating own templates), use the Meta slide class, which can access the
 * presentation object and state to generate a slide.
 *
 * The only keys used in the packaged templates are currently "author" and "title" in the `header` template */
val meta = Map(
  "author" -> "Alexander Gehrke",
  "title" -> "Git"
)

/* A presentation consists of a `Vector[Slide]`. After each slide in this vector, the presentation pauses and waits for
 * input. `Group` is used, to display several slides at once, and `PauseKey` can be used inside a `Group` to wait for
 * input (but limited to displaying the rest of the `Group`, normal navigation is only possible between top level 
 * slides).
 *
 * Navigation distinguishes between normal and "quick" steps, the latter disables things like pauses or animations. It
 * is also possible to jump to a specific slide.
 * */
val presentation = Presentation(Vector(
  titleSlide,
  /* for simple slides, the `markdown` template and `IncludeMarkdown` Slide allow including plain markdown files.
   * (currently requires mdcat) */
  markdown("Wozu braucht man das?", pwd/"slides"/"01wozu.md"),
  chapter("Basics", "Grundlegende Befehle"),
  /* `slide` is a template for a `Group` with an optional title, that clears the screen */
  slide("Basics")(
    """Git trackt Inhalt von Dateien innerhalb eines Repositories.
      |Kann in neuem oder bestehenden Ordner angelegt werden
      |""".par,
    /* the `---` template displays a yellow line */
    ---,
    /* `Pause` waits for the given number of milliseconds */
    Pause(500),
    /* `TypedCommand.shell` (here aliased to `sh`) displays a typing animation of that command and then executes it,
     * showing its output in the presentation */
    sh("git init demoRepo")(using pwd),
    /* sometimes it's useful to display something else than is actually executed, e.g. to add comments, or to hide
     * options only required because we don't call from an interactive shell (stuff like --color) */
    sh("ls -1a --color=always demoRepo")(using pwd) showing "ls -1a demoRepo",
    /* If you need to run commands that should not display anything in the presentation, use `Silent`.
     * Here I use it to prepare the repository, but it could also be used e.g. for playing a sound or opening a video.*/
    Silent {
      %%%("git", "config", "user.email", "crater2150@example.com")
      %%%("git", "config", "user.name", "crater2150")
      %%%("git", "config", "color.status", "always")
      %%%("git", "config", "color.ui", "always")
    }
  ),
  slide("Basics")(
    "Noch ist das Repo leer. Ändern wir das:".par,
    ---,
    Pause(500),
    TypedCommand.fake("cd demoRepo"),
    sh("echo 'Hello, World!' > hello.txt"),
    Pause(500),
    sh("ls -1a")
    ),
  slide("Basics")(
    "Den Zustand des Repos sehen wir mit `git status`\n".par,
    ---,
    Pause(500),
    gitStatus,
  ),
  slide("Basics")(
    "Damit Git Dateien trackt, müssen diese mit `git add` hinzugefügt werden.\n".par,
    ---,
    sh("git add hello.txt"),
    gitStatus,
  ),
  slide("Basics")(
    """Die gespeicherten Zustände eines Repos nennt man Commit.
      |Zu jedem Commit gibt man eine Zusammenfassung der Änderungen an
      |""".par,
    ---,
    Pause(500),
    sh("git commit --message \"Added first file\""),
    PauseKey,
    ---,
    """Commits werden über ihren SHA1-Hash identifiziert. Dieser wird ggf. abgekürzt.
      |Mit `git log` können wir uns die bisherigen Commits ansehen:
      |""".par,
    ---,
    sh("git log")
  ),
  /* here we see `gitHashes`, created with `prepare`, in action:
   * As the code is run exactly once per `useIn` call i.e. per using slide, the hashes of the latest commit, tree and
   * parent in the demo repo, that `gitHashes` retrieves, are always the ones for the repository state matching the
   * slide. The prepared code is run and its result cached, when the slide is first displayed (or skipped over).
   */
  gitHashes useIn { sha => slide("Was passiert bei einem Commit?")(
    s"""Schauen wir uns an, was Git aus dem Commit gemacht hat.
       |`git cat-file $$ref` zeigt Inhalt von Git-Objekten. `$$ref` ist Referenz des Objekts, z.B. der Hash.
       |Unser Commit hatte den Hash `${sha.commit}`.
       |Statt diesem geht auch `HEAD` = aktueller Commit.
       |""".par,
    ---,
    gitCatFile("HEAD"),
    PauseKey,
    ---,
    s"""Zum Hashen wird vor den Inhalt noch `<Objekttyp> <Länge des Inhalts in bytes\\0` gehängt.
    |Hier: `Objekttyp == commit`
    |""".par,
  )},
  gitHashes useIn { sha => slide("Commits, Trees, Blobs")(
    s"""Neben den Metadaten ist in unserem Commit noch eine Zeile
       |  `tree ${sha.tree}`
       |
       |Trees repräsentieren Verzeichnisse, Tree in einem Commit = Wurzelverzeichnis des Repos
       |`$$ref^{tree}` = Baum mit Referenz `$$ref`, oder wenn `$$ref` ein Commit ist, der Tree aus diesem
       |""".par,
    ---,
    gitCatFile("HEAD^{tree}"),
    PauseKey,
    ---,
    s"""Hier ist noch nicht viel zu sehen, da wir nur eine Datei haben.
       |Einträge im Tree haben das Format:
       |   `<berechtigungen> <blob | tree> <sha1 hash>    <dateiname>`
       |""".par
  )},
  gitHashes useIn { sha => slide("Commits, Trees, Blobs")(
    s"""Blobs sind die eigentlichen Dateiinhalte.
       |Unsere `hello.txt` hat, wie im Tree zu sehen, den Hash ${sha.blob.substring(0,8)}…
       |""".par,
    ---,
    gitCatFile(sha.blob.substring(0,8)),
  )},
  slide("Commits, Trees, Blobs")(
    s"""Fügen wir ein paar weitere Dateien und einen Unterordner hinzu
       |""".par,
    ---,
    sh("mkdir folder"),
    sh("echo 'My other file' > other.txt"),
    sh("echo 'File in folder' > folder/file.txt"),
    sh("git add other.txt folder"),
    sh("git commit -m 'Added more files'")
  ),
  /* when this slide is reached, our commit hashes have changed. All the previous slides will still display the old ones */
  gitHashes useIn { sha => slide("Commits, Trees, Blobs")(
    s"""Wie sieht der neue Commit aus?
       |""".par,
    ---,
    gitCatFile("HEAD"),
    ---,
    PauseKey,
    s"""Wir haben eine neue Art von Eintrag:
       |  `parent ${sha.parent}`
       |
       |Dieser verweist auf den vorherigen Commit.
       |Durch solche Verweise bilden alle Commits einen gerichteten Graphen.
       |""".par
  )},
  gitHashes useIn { sha => slide("Commits, Trees, Blobs")(
    s"""Sehen wir uns den neuen Tree an:
       |""".par,
    gitCatFile("HEAD^{tree}"),
    ---,
    s"""Eine unserer neuen Dateien ist zu sehen.
       |Auch `hello.txt` ist noch da, mit selbem Hash wie vorher (da nicht geändert)
       |Die andere neue Datei ist im Verzeichnis `folder`, und daher im `tree` mit dem Hash `${sha.subtree.substring(0,8)}…` :
       |""".par,
    gitCatFile(sha.subtree.substring(0,8)),
  )},
  slide("Git als Graph")(Image(imgs/"repo-graph.png", height=imageHeight)),
  slide("Dateien editieren")(
    s"""Auch Änderungen an schon getrackten Dateien müssen mit `git add` zum Repo hinzugefügt werden.
       |Erlaubt es, nur Teile der Änderungen zu committen und dadurch commits zu unterteilen.
       |Ändern wir ein paar Dateien:
       |""".par,
       PauseKey,
       Group(
         interactive("vim", "hello.txt").replaceIf(debug)(sh("echo 'New line' >> hello.txt")),
         interactive("vim", "folder/file.txt").replaceIf(debug)(sh("echo 'New line' >> folder/file.txt")),
         gitStatus,
       ),
  ),
  slide("Dateien editieren")(
    sh("git add folder/file.txt; git status"),
    ---,
    s"""`git status` zeigt, welche Änderungen in den nächsten Commit aufgenommen werden.
       |Solche bezeichnet man als "staged" oder "im Stagingbereich".
       |Auch zu sehen: Befehle um diese zu modifizieren (`git add` / `git restore --staged`).
       |`git restore <file>` (ohne `--staged`) verwirft Änderungen →  Nicht wieder herstellbar!
       |""".par,
  ),
  slide("Dateien editieren")(
    sh("git commit -m \"Modified file in folder\""),
    sh("git add hello.txt; git commit -m \"Extended greeting\""),
    gitStatus,
  ),
  markdown("Was bisher geschah", pwd/"slides"/"02summary.md"),
  chapter("Branches", "Grundlegende Befehle"),
  slide("\n".par, IncludeMarkdown(pwd/"slides"/"02branches.md")),
  slide("Branches")(
    sh("git branch"),
    ---,
    prepare { %%%("git", "branch", "--show-current").trim } useIn { branch => 
      s"Aktuell sind wir auf dem Branch `$branch` und es gibt keine weiteren Branches\n".par
    },
    PauseKey,
    s"""Neuen Branch anlegen mit `git branch <name>`
       |Alternativ: Branch anlegen und direkt auf diesen wechseln mit `git switch -c <name>`
       |""".par,
    ---,
    sh("git switch -c feature/foo"),
    sh("git branch"),
  ),
  slide("Branches")(
    "Machen wir ein paar Änderungen und committen sie:\n".par,
    ---,
    sh("echo 'a new line' >> hello.txt") showing "echo 'a new line' >> hello.txt  # appends to hello.txt",
    sh("git add hello.txt"),
    sh("git commit -m \"Added line to hello\""),
    ---,
    "Da der Branch `feature/foo` aktiv war, zeigt dieser auf den neuen Commit. `master` wird nicht geändert:".code.text,
    PauseKey,
    sh("git log --graph --oneline --all --decorate=short --color=always") showing "git log --graph --oneline --all",
  ),
  slide("Branches")(
    "Jetzt wechseln wir zurück zu `master`:\n".par,
    ---,
    sh("git switch master"),
    sh("echo 'Also a new line' >> other.txt"),
    sh("git add other.txt"),
    sh("git commit -m \"Added line to other\""),
    gitLogGraph(),
  ),
  slide("Branches")(
    "Im Prinzip haben wir einfach zwei Commits mit dem selben `parent` hinzugefügt:".code.text,
    ---,
    gitCatFile("master"),
    gitCatFile("feature/foo"),
  ),
  slide("Merges")(
    """Im Normalfall möchte man mehrere Branches irgendwann wieder zusammenführen.
      |Dazu gibt es den Befehl `merge`:""".par,
    ---,
    sh("git merge feature/foo"),
    gitLogGraph(2),
  ),
  slide("Merge Commits")(
    "Um zwei divergierte Branches zusammenzuführen, gibt es Merge Commits mit mehreren `parent` Einträgen:".code.text,
    ---,
    gitCatFile("master"),
  ),
  slide("Merge Konflikte")(
    "Was passiert, wenn wir nicht kompatible Änderungen haben? Noch mal vom Zustand vor dem Merge aus, auf `master`:".code.text,
    Silent {
      %%%("git", "update-ref", "refs/heads/master", "feature/foo~")
    },
    ---,
    sh("echo 'a different line' >> hello.txt"),
    sh("git add hello.txt"),
    sh("git commit -m \"Added different line to hello\""),
    gitLogGraph(),
  ),
  slide("Der Object Store")(
    """Git speichert Objekte unter `.git/objects/<erste zwei Stellen vom Hash>/<Rest vom Hash>`
      |Objekte sind komprimiert.
      |""".par,
    sh("tree -C .git/objects/ | head -n 11") showing "tree .git/objects/",
    s"...".par,
  ),
), meta=meta)

/* When starting the presentation, you can pass a custom keymap to use.
 * The `runForeground` action lets you run any command, interrupting the presentation until it exits.
 * Here I bind the 'i' key to open tmux in the demo repo, for having interactive access, so I can call additional git
 * commands for questions */
presentation.start(using Keymap.default ++ Map(
  Key('i') -> SlideAction.runForeground("tmux"),
))


/* vim:set tw=120: */
