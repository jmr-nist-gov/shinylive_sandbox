modal_state_info <- function(n_aliquots, state, n_species) {
  modalDialog(
    title = sprintf("These %s aliquots from %s include %s species.",
                    scales::comma(n_aliquots),
                    state,
                    scales::comma(n_species)),
    DTOutput('modal_state_info_dt'),
    size = "l",
    easyClose = TRUE
  )
}