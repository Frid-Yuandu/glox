export function do_exit(code = 0) {
  // Check if the current environment is Deno
  const isDeno = typeof Deno !== 'undefined';

  if (isDeno) {
    // If running in Deno, use Deno.exit
    Deno.exit(code);
  } else {
    // If running in Node.js or Bun, use process.exit
    if (typeof process !== 'undefined' && typeof process.exit === 'function') {
      process.exit(code);
    } else {
      throw new Error('Unsupported runtime: unable to exit the process.');
    }
  }
}
