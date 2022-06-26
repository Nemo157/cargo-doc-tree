#![feature(exit_status_error)]

use anyhow::{Context, Error};
use fn_error_context::context;

mod print;

#[fehler::throws]
fn build_docs_json() -> camino::Utf8PathBuf {
    let metadata = cargo_metadata::MetadataCommand::new().exec()?;

    std::process::Command::new("cargo")
        .args(&[
            "rustdoc",
            "--all-features",
            "--",
            "--output-format=json",
            "-Zunstable-options",
        ])
        .status()
        .context("Couldn't get cargo's exit status")?
        .exit_ok()
        .context("building docs failed")?;

    let mut path = metadata.target_directory;
    path.push("doc");
    let resolve = metadata.resolve.context("missing resolve")?;
    let root_id = resolve.root.context("missing root id")?;
    let root = metadata
        .packages
        .into_iter()
        .find(|pkg| pkg.id == root_id)
        .context("missing root package")?;
    let root_target = root
        .targets
        .into_iter()
        .next()
        // .find(|target| target.kind.iter().any(|kind| kind == "lib"))
        .map(|target| target.name)
        // .or_else(root.default_run)
        .context("missing default target")?
        .replace("-", "_");
    path.push(root_target);
    path.set_extension("json");
    path
}

#[fehler::throws]
#[context("could not print crate")]
fn print_crate(mut output: impl std::io::Write, krate: &rustdoc_types::Crate) {
    write!(output, "{}", print::P::new(krate, &krate.root))?;
}

#[fehler::throws]
fn main() {
    let json_path = build_docs_json()?;
    let json_file = std::fs::File::open(&json_path).with_context(|| format!("Could not open {json_path}"))?;
    let krate: rustdoc_types::Crate = serde_json::from_reader(json_file)?;
    let stdout = std::io::stdout();
    print_crate(stdout.lock(), &krate)?;
}
