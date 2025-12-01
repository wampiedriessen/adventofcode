use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("all-aoc-puzzles.rs");

    let mut uses = String::from("use aoc_util::PuzzleDay;\nuse std::time::SystemTime;\n");
    let mut main = String::new();

    for d in 1..=25 {
        let day = format!("{:0>2}", d);
        let path = format!("../puzzle-inputs/day{day}.txt");
        let path =  Path::new(&path);
        if path.exists() && Path::new(&format!("../day{day}/src/lib.rs")).exists() {
            let path = path.canonicalize().unwrap();
            let path = path.to_str().unwrap();
            uses += &format!("use day{day}::Day{day};\n");
            main += &format!(r#"
            let mut solution = Day{day}::new(include_str!(r"{path}"));
            let start = SystemTime::now();
            let answer = solution.solve();
            println!("🎅 Running Day {day} (took {{}} ms):", start.elapsed().unwrap().as_millis());
            if let Ok((a1, a2)) = answer {{
                println!("  ⭐  Part 1: {{}}", a1);
                println!("  ⭐  Part 2: {{}}", a2);
            }} else {{
                println!("💥 Could not find solution for day {day}");
            }}
"#);
            // Recompile on input change
            println!("cargo::rerun-if-changed=../puzzle-inputs/day{day}.txt");
        } else {
            main += &format!(r#"println!("⚠️ No input or lib file found for day {day}");"#);
        }
    }

    fs::write(&dest_path, format!("{uses}
fn all_aoc_puzzles() {{
    {main}
}}")).unwrap();
}