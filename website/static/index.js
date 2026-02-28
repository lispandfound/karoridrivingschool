document.addEventListener("DOMContentLoaded", () => {
    fetch("/api/reviews")
        .then((response) => response.json())
        .then((reviews) => {
            console.log(reviews);
            const reviewTitle = document.getElementById("review-title");
            const reviewBody = document.getElementById("review-body");
            if (reviews.length > 0) {
                reviewTitle.textContent = reviews[0].title;
                reviewBody.textContent = reviews[0].body;
            }
            const prevBtn = document.getElementById("prev-review-btn");
            const nextBtn = document.getElementById("next-review-btn");
            let currentSlideIndex = 0;
            function showSlide(index) {
                index = (index + reviews.length) % reviews.length;
                reviewTitle.textContent = `Review from ${reviews[index].name}`;
                reviewBody.textContent = reviews[index].review;
                currentSlideIndex = index;
            }
            showSlide(currentSlideIndex);
            function nextSlide() {
                showSlide(currentSlideIndex + 1);
            }
            function prevSlide() {
                showSlide(currentSlideIndex - 1);
            }
            if (nextBtn) {
                nextBtn.addEventListener("click", nextSlide);
            }
            if (prevBtn) {
                prevBtn.addEventListener("click", prevSlide);
            }
        });
});
