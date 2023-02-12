use std::io::prelude::*;
use std::time::Duration;

mod toml;

struct Job {
    cmd: Vec<String>,
    freq: Duration,
    logfile: Option<String>,
}

fn main() {
    while let Err(err) = attempt() {
        log::error!("{}", err);
        std::thread::sleep(Duration::from_secs(3));
    }
}

fn attempt() -> Result<(), String> {
    env_logger::init();

    let config_file = std::fs::read_to_string(
        std::env::args()
            .collect::<Vec<_>>()
            .get(1)
            .ok_or_else(|| String::from("need config file arg"))?,
    )
    .map_err(|_| "can't read config file")?;

    let toml = toml::parse(&config_file).map_err(|_| String::from("invalid toml"))?;

    let mut jobs = Vec::new();
    for job in toml
        .get("job")
        .ok_or_else(|| String::from("missing `job` array"))?
        .as_list()
        .ok_or_else(|| String::from("`job` must be an array"))?
    {
        let toml::Value::Array { values: cmd, .. }
            = job.get("cmd").ok_or_else(|| String::from("`job` must have `cmd`"))?
        else {
            panic!("`job.cmd` must be array of strings");
        };
        if !cmd.iter().all(|el| el.is_str()) {
            panic!("`job.cmd` must be array of strings");
        }
        if cmd.is_empty() {
            panic!("`job.cmd` must have at least one element");
        }
        let cmd = cmd.iter().map(|el| el.as_str().unwrap().into()).collect();

        let toml::Value::String { value: freq, .. }
            = job.get("every").ok_or_else(|| String::from("`job` must have `every`"))?
        else {
            panic!("`job.every` must be string");
        };

        let freq = *freq
            .parse::<humantime::Duration>()
            .map_err(|_| String::from("`job.freq` is not a valid duration"))?;

        let logfile = job
            .get("logfile")
            .map(|logfile| {
                logfile
                    .as_str()
                    .ok_or_else(|| String::from("`job.logfile` must be string"))
                    .map(|s| s.into())
            })
            .transpose()?;

        jobs.push(Job { cmd, freq, logfile });
    }

    let handles = jobs
        .into_iter()
        .map(|job| {
            std::thread::spawn(move || {
                let mut cmd = std::process::Command::new(&job.cmd[0]);
                job.cmd.iter().skip(1).for_each(|part| {
                    cmd.arg(part);
                });
                cmd.stdout(std::process::Stdio::piped());
                cmd.stderr(std::process::Stdio::piped());
                cmd.stdin(std::process::Stdio::null());

                loop {
                    log::info!("starting {:?}", cmd);

                    let Ok(mut child) = cmd.spawn().map_err(|e| log::error!("failed to run: {}", e)) else { return };
                    let mut child_stdout = child.stdout.take().unwrap();
                    let mut child_stderr = child.stderr.take().unwrap();

                    let log1 = job.logfile.clone();
                    let log2 = job.logfile.clone();

                    std::thread::spawn(move || {
                        let mut writer_out: Box<dyn Write> = if let Some(logfile) = log1 {
                            Box::new(std::io::LineWriter::new(
                                std::fs::File::options()
                                    .create(true)
                                    .append(true)
                                    .open(logfile)
                                    .expect("couldn't open logfile"),
                            ))
                        } else {
                            Box::new(std::io::stdout())
                        };

                        std::io::copy(&mut child_stdout, &mut writer_out)
                            .expect("couldn't copy stdout");
                    });

                    std::thread::spawn(move || {
                        let mut writer_err: Box<dyn Write> = if let Some(logfile) = log2 {
                            Box::new(std::io::LineWriter::new(
                                std::fs::File::options()
                                    .create(true)
                                    .append(true)
                                    .open(logfile)
                                    .expect("couldn't open logfile"),
                            ))
                        } else {
                            Box::new(std::io::stdout())
                        };

                        std::io::copy(&mut child_stderr, &mut writer_err)
                            .expect("couldn't copy stderr");
                    });

                    let Ok(_) = child
                        .wait()
                        .map_err(|_| log::error!("couldn't wait on job")) else {return};

                    log::info!("finishing {:?}", cmd);
                    std::thread::sleep(job.freq);
                }
            })
        })
        .collect::<Vec<_>>();

    for handle in handles {
        handle
            .join()
            .map_err(|_| String::from("coudln't join child thread"))?;
    }

    Ok(())
}
