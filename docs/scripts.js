document.addEventListener('DOMContentLoaded', function() {
    const headers = document.querySelectorAll('.sidebar-header');
    headers.forEach(header => {
        header.addEventListener('click', function() {
            const section = header.nextElementSibling;
            section.classList.toggle('collapsed');
        });
    });
});
