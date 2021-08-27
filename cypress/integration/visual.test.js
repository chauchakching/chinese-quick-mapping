describe('visual test', () => {
  it('homepage', () => {
    cy.visit('http://localhost:8080')

    cy.document().toMatchImageSnapshot()
  })
})