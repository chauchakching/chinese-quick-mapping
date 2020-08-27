module.exports = {
  future: {
    removeDeprecatedGapUtilities: true,
  },
  purge: {
    enabled: true,
    content: [
      // './src/**/*.html',
      './src/**/*.elm',
      // './src/**/*.js',
    ],
  },
  theme: {
    extend: {},
  },
  variants: {},
  plugins: [],
}
