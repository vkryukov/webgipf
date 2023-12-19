/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./src/**/*.html", "./src/**/*.js", "./*.html", "./*.js", "../server/static/*.html",
    "../server/static/*.js"],
  theme: {
    extend: {
      fontFamily: {
        'sitename': ['Rubik Mono One', 'sans-serif']
      }
    }
  },
  plugins: [],
}

